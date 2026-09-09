package rio

import spinal.core._
import spinal.lib._

/** Common mesh wiring for the legacy fixed-latency and staged CAM backends.
  * The legacy response remains a Flow converted to Stream; its existing callers
  * retain their buffers. The staged backend honors ready in both directions.
  */
case class MeshCamLookup(inputWidth: Int, outputWidth: Int, config: EngineConfig,
                         replay: Boolean) extends Area {
  def updater = MapperUpdater(inputWidth, outputWidth)
  val io = new Bundle {
    val readReq = Stream(UInt(inputWidth bits))
    val readRes = Stream(UInt(outputWidth bits))
    val writeReq = Stream(updater)
    val commit, commitReady, capacityBlocked = Bool()
  }
  val activeBank: Bool = if (config.pipelinedCam) {
    val core = PipelinedCamMapper(inputWidth, outputWidth, config.camCapacity,
      replay, config.camEntriesPerStage)
    core.io.readReq << io.readReq
    io.readRes << core.io.readRes
    core.io.writeReq << io.writeReq
    core.io.commit := io.commit
    io.commitReady := core.io.commitReady
    io.capacityBlocked := core.io.capacityBlocked
    core.activeBank
  } else {
    val core: ConfigurableMapper = if (config.usesCam)
      BoundedCamMapper(inputWidth, outputWidth, config.camCapacity, replay)
    else if (replay) ReplayMapper(inputWidth, outputWidth)
    else DirectMapper(inputWidth, outputWidth)
    core.io.readReq << io.readReq.toFlow
    io.readRes << core.io.readRes.toStream
    core.io.writeReq << io.writeReq
    core.io.commit := io.commit
    io.commitReady := core.io.commitReady
    io.capacityBlocked := core.io.capacityBlocked
    core.activeBank
  }
}

/** Reserve response storage before issuing an unstallable synchronous RAM read.
  * Credits cover both the RAM pipeline and the response FIFO, so an arbitrarily
  * stalled downstream consumer cannot lose or misalign a response.
  */
object BufferedMapperRead {
  def apply(request: Stream[UInt], readReq: Flow[UInt], readRes: Flow[UInt],
            depth: Int): Stream[UInt] = {
    val area = new Area {
      val used = Reg(UInt(log2Up(depth + 1) bits)) init(0)
      val responses = new StreamFifo(cloneOf(readRes.payload), depth,
        withAsyncRead = true, withBypass = true, useVec = true)
      request.ready := used < depth
      readReq.valid := request.fire
      readReq.payload := request.payload
      responses.io.push.valid := readRes.valid
      responses.io.push.payload := readRes.payload
      when(request.fire =/= responses.io.pop.fire) {
        when(request.fire) { used := used + 1 }.otherwise { used := used - 1 }
      }
      assert(!readRes.valid || responses.io.push.ready, "mapper response credit overflow")
    }
    area.responses.io.pop
  }
}
