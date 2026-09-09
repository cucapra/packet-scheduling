package rio

import spinal.core._
import spinal.lib._

/** Unmodified, MIT-licensed alexforencich/verilog-cam SRL core.
  * See hw/verilog/vendor/verilog-cam/UPSTREAM.json for the pinned revision.
  * Its match vector is registered; its priority encoder is combinational.
  */
private[rio] case class SrlCamBlock(keyWidth: Int, addressWidth: Int, sliceWidth: Int) extends BlackBox {
  addGeneric("DATA_WIDTH", keyWidth)
  addGeneric("ADDR_WIDTH", addressWidth)
  addGeneric("SLICE_WIDTH", sliceWidth)
  val io = new Bundle {
    val clk, rst = in Bool()
    val write_addr = in UInt(addressWidth bits)
    val write_data = in UInt(keyWidth bits)
    val write_delete, write_enable = in Bool()
    val write_busy = out Bool()
    val compare_data = in UInt(keyWidth bits)
    val match_many, match_single = out Bits((1 << addressWidth) bits)
    val match_addr = out UInt(addressWidth bits)
    val `match` = out Bool()
  }
  noIoPrefix()
  mapCurrentClockDomain(io.clk, io.rst)
  setDefinitionName("cam_srl")
  addRTLPath("hw/verilog/vendor/verilog-cam/rtl/cam_srl.v")
  addRTLPath("hw/verilog/vendor/verilog-cam/rtl/priority_encoder.v")
}

private[rio] case class CamPipelineSearch(keyWidth: Int, addressWidth: Int) extends Bundle {
  val key = UInt(keyWidth bits)
  val forWrite, hit, hasFree = Bool()
  val address, freeAddress = UInt(addressWidth bits)
}

private[rio] object CamPipelineWriteState extends SpinalEnum {
  val Init, Idle, Probe, Search, Drain, Launch, Updating, Ack, Full = newElement()
}

/** Optional staged exact-match mapper; deliberately separate from BoundedCamMapper.
  *
  * Each stage contains at most entriesPerStage entries (normally 128 or 256),
  * using an upstream SRL CAM per bank. The key, match address and first free slot
  * advance one stage/cycle. The final address reads one synchronous value RAM.
  * With an unstalled consumer, latency is stageCount + 1 cycles and initiation
  * interval is one cycle. A credit-reserved register FIFO absorbs output stalls.
  * This Stream interface must NOT be connected via toFlow to the existing
  * one-cycle mapper callers: they must honor ready and align packet metadata.
  *
  * A zero write deletes; a missing key reads zero. There is no eviction. A full
  * new-key write holds ready low (and capacityBlocked high) until reset; an
  * ordered producer must delete before inserting if capacity would be exceeded.
  *
  * Replay duplicates the index and value bank only. The existing control FIFO
  * remains responsible for replay; this module has no command journal. Write
  * payload/valid must remain stable until ready, which acknowledges COMPLETION,
  * including the upstream multi-cycle update. In particular, upstream cam_srl
  * uses its write_addr input throughout write_busy, so it is held stable here.
  * Existing-key value changes bypass the SRL rewrite entirely.
  *
  * Requests retain their bank by entering that bank's pipeline. A write drains
  * that bank's old requests before modifying it. Replay allows active-bank reads
  * during shadow writes; static writes temporarily stop admission. Commit only
  * fires when commitReady is high; reads accepted on its edge use the old bank.
  * Reset flushes requests and waits for all upstream cores to initialize.
  */
case class PipelinedCamMapper(inputWidth: Int, outputWidth: Int, capacity: Int,
                              replay: Boolean, entriesPerStage: Int = 128,
                              sliceWidth: Int = 4) extends Component {
  require(inputWidth > 0 && outputWidth > 0)
  require(capacity >= 2 && BigInt(capacity) <= (BigInt(1) << inputWidth))
  require(entriesPerStage >= 2 && isPow2(entriesPerStage))
  require(sliceWidth == 4 || sliceWidth == 5, "SRL16 or SRL32 slices")
  val stageCount = (capacity + entriesPerStage - 1) / entriesPerStage
  val minimumReadLatency = stageCount + 1
  private val addressWidth = log2Up(capacity)
  private val bankCount = if (replay) 2 else 1
  private val responseDepth = minimumReadLatency + 2
  val io = new Bundle {
    val readReq = slave Stream(UInt(inputWidth bits))
    val readRes = master Stream(UInt(outputWidth bits))
    val writeReq = slave Stream(MapperUpdater(inputWidth, outputWidth))
    val commit = in Bool()
    val commitReady, capacityBlocked, initialized = out Bool()
  }
  val activeBank: Bool = if (replay) RegInit(False) else False
  private val state = Reg(CamPipelineWriteState()) init(CamPipelineWriteState.Init)
  private val writeBank = RegInit(False)
  private val writeAddress = Reg(UInt(addressWidth bits)) init(0)
  private val mutate = RegInit(False)
  private val rewriteIndex = RegInit(False)
  private val deleting = io.writeReq.outputId === 0
  private val commitFire = io.commit && io.commitReady
  private val outstanding = Reg(UInt(log2Up(responseDepth + 1) bits)) init(0)

  io.initialized := state =/= CamPipelineWriteState.Init
  io.commitReady := state === CamPipelineWriteState.Idle
  io.writeReq.ready := state === CamPipelineWriteState.Ack
  io.capacityBlocked := state === CamPipelineWriteState.Full
  io.readReq.ready := io.initialized && outstanding < responseDepth &&
    (if (replay) True else state === CamPipelineWriteState.Idle || state === CamPipelineWriteState.Full)
  when(io.readReq.fire =/= io.readRes.fire) {
    when(io.readReq.fire) { outstanding := outstanding + 1 }
      .otherwise { outstanding := outstanding - 1 }
  }
  if (replay) when(commitFire) { activeBank := !activeBank }

  private val banks = (0 until bankCount).map { bankId => new Area {
    val selectedRead = if (!replay) True else if (bankId == 0) !activeBank else activeBank
    val selectedWrite = if (bankId == 0) !writeBank else writeBank
    val occupied = Vec.fill(capacity)(RegInit(False))
    val values = Mem(UInt(outputWidth bits), capacity)
    val input = Flow(CamPipelineSearch(inputWidth, addressWidth))
    val probing = state === CamPipelineWriteState.Probe && selectedWrite
    input.valid := (io.readReq.fire && selectedRead) || probing
    input.key := Mux(probing, io.writeReq.inputId, io.readReq.payload)
    input.forWrite := probing
    input.hit := False
    input.address := 0
    input.hasFree := False
    input.freeAddress := 0
    // A shadow probe may follow old-bank reads in this pipeline, but no mutation
    // is allowed until every preceding read has captured its value RAM result.
    assert(!(probing && io.readReq.fire && selectedRead))

    var previous = input
    val stages = (0 until stageCount).map { stageId => new Area {
      val start = stageId * entriesPerStage
      val count = scala.math.min(entriesPerStage, capacity - start)
      val localAddressWidth = scala.math.max(1, log2Up(count))
      val core = SrlCamBlock(inputWidth, localAddressWidth, sliceWidth)
      val incoming = previous
      val aligned = RegNext(incoming.payload)
      val valid = RegNext(incoming.valid) init(False)
      val freeBits = Bits(count bits)
      for (i <- 0 until count) freeBits(i) := !occupied(start + i)
      val freeOneHot = OHMasking.first(freeBits)
      val localFreeAddress = if (count == 1) U(0, addressWidth bits)
        else OHToUInt(freeOneHot).resize(addressWidth)
      val hasFree = RegNext(freeBits.orR) init(False)
      val freeAddress = RegNext(U(start, addressWidth bits) + localFreeAddress)

      core.io.compare_data := incoming.key
      core.io.write_addr := writeAddress.resize(localAddressWidth)
      core.io.write_data := io.writeReq.inputId
      core.io.write_delete := deleting
      val belowEnd = if (BigInt(start + count) == (BigInt(1) << addressWidth)) True
        else writeAddress < start + count
      core.io.write_enable := state === CamPipelineWriteState.Launch && selectedWrite &&
        writeAddress >= start && belowEnd

      val output = Flow(CamPipelineSearch(inputWidth, addressWidth))
      output.valid := valid
      output.key := aligned.key
      output.forWrite := aligned.forWrite
      output.hit := aligned.hit || core.io.`match`
      output.address := Mux(aligned.hit, aligned.address,
        U(start, addressWidth bits) + core.io.match_addr.resize(addressWidth))
      output.hasFree := aligned.hasFree || hasFree
      output.freeAddress := Mux(aligned.hasFree, aligned.freeAddress, freeAddress)
      previous = output
    }}
    val tail = previous
    val readValid = RegNext(tail.valid && !tail.forWrite) init(False)
    val readHit = RegNext(tail.valid && !tail.forWrite && tail.hit) init(False)
    val readData = values.readSync(tail.address, tail.valid && !tail.forWrite && tail.hit)
    val result = Mux(readHit, readData, U(0, outputWidth bits))
    val readsInFlight = readValid || stages.map(s => s.valid && !s.aligned.forWrite).reduce(_ || _)
    val indexBusy = stages.map(_.core.io.write_busy).reduce(_ || _)
    val writeEnable = io.writeReq.fire && selectedWrite && mutate && !deleting
    values.write(writeAddress, io.writeReq.outputId, writeEnable)
    when(io.writeReq.fire && selectedWrite && mutate) {
      occupied(writeAddress) := !deleting
    }
    when(state === CamPipelineWriteState.Search && tail.valid && tail.forWrite) {
      writeAddress := Mux(tail.hit, tail.address, tail.freeAddress)
      mutate := tail.hit || !deleting
      rewriteIndex := !tail.hit || deleting
      when(!tail.hit && !tail.hasFree && !deleting) {
        state := CamPipelineWriteState.Full
      } elsewhen(!tail.hit && deleting) {
        state := CamPipelineWriteState.Ack
      } otherwise {
        state := CamPipelineWriteState.Drain
      }
    }
  }}

  private val allIndexesReady = !banks.map(_.indexBusy).reduce(_ || _)
  private val targetReadsInFlight = if (replay)
    Mux(writeBank, banks(1).readsInFlight, banks(0).readsInFlight) else banks.head.readsInFlight
  private val targetIndexBusy = if (replay)
    Mux(writeBank, banks(1).indexBusy, banks(0).indexBusy) else banks.head.indexBusy
  switch(state) {
    is(CamPipelineWriteState.Init) {
      when(allIndexesReady) { state := CamPipelineWriteState.Idle }
    }
    is(CamPipelineWriteState.Idle) {
      when(io.writeReq.valid && !commitFire) {
        writeBank := (if (replay) !activeBank else False)
        state := CamPipelineWriteState.Probe
      }
    }
    is(CamPipelineWriteState.Probe) { state := CamPipelineWriteState.Search }
    is(CamPipelineWriteState.Drain) {
      when(!targetReadsInFlight) {
        state := Mux(rewriteIndex, CamPipelineWriteState.Launch, CamPipelineWriteState.Ack)
      }
    }
    is(CamPipelineWriteState.Launch) { state := CamPipelineWriteState.Updating }
    is(CamPipelineWriteState.Updating) {
      when(!targetIndexBusy) { state := CamPipelineWriteState.Ack }
    }
    is(CamPipelineWriteState.Ack) {
      when(io.writeReq.fire) { state := CamPipelineWriteState.Idle }
    }
  }

  private val response = Stream(UInt(outputWidth bits))
  response.valid := banks.map(_.readValid).reduce(_ || _)
  response.payload := (if (replay) Mux(banks(1).readValid, banks(1).result, banks(0).result)
                       else banks.head.result)
  // No additional RAM type: the small elastic response buffer uses registers.
  private val responses = new StreamFifo(UInt(outputWidth bits), responseDepth,
    withAsyncRead = true, withBypass = true, useVec = true)
  responses.io.push << response
  io.readRes << responses.io.pop
  assert(!response.valid || response.ready, "reserved response storage overflow")
  if (replay) {
    assert(!(banks(0).readValid && banks(1).readValid))
    assert(!(banks(0).writeEnable && banks(1).writeEnable))
  }
  when(state =/= CamPipelineWriteState.Idle && state =/= CamPipelineWriteState.Init) {
    assert(io.writeReq.valid, "hold writeReq valid/payload until completion")
  }
}

/** Standalone elaboration, without changing the existing mesh synthesis default.
  * Usage: GeneratePipelinedCam OUTPUT_DIR CAPACITY ENTRIES_PER_STAGE [static|replay]
  */
object GeneratePipelinedCam {
  def main(args: Array[String]): Unit = {
    require(args.length >= 3 && args.length <= 4)
    val mode = args.lift(3).getOrElse("replay")
    require(Set("static", "replay").contains(mode))
    val capacity = args(1).toInt
    val perStage = args(2).toInt
    Config.spinal.copy(targetDirectory = args(0)).generateVerilog(
      PipelinedCamMapper(23, 13, capacity, mode == "replay", perStage))
    val output = java.nio.file.Paths.get(args(0))
    val vendor = java.nio.file.Paths.get("hw/verilog/vendor/verilog-cam")
    for (filename <- Seq("cam_srl.v", "priority_encoder.v")) {
      java.nio.file.Files.copy(vendor.resolve("rtl").resolve(filename), output.resolve(filename),
        java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    }
    java.nio.file.Files.copy(vendor.resolve("COPYING"), output.resolve("COPYING.verilog-cam"),
      java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    java.nio.file.Files.copy(vendor.resolve("UPSTREAM.json"), output.resolve("verilog-cam-UPSTREAM.json"),
      java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    java.nio.file.Files.writeString(output.resolve("rtl-files.f"),
      "PipelinedCamMapper.v\ncam_srl.v\npriority_encoder.v\n")
    println(s"Pipelined CAM: $capacity entries/bank, $perStage entries/stage, " +
      s"${(capacity + perStage - 1) / perStage + 1} cycles minimum lookup latency, $mode")
  }
}
