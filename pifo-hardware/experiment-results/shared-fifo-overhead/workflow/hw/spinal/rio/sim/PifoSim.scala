package rio.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import rio._

object BasicPifoSim extends App {
  
  // Create a test configuration for the PIFO
  val testConfig = PifoConfig(
    numPifo = 64,    // Smaller size for easier testing
    bitPort = 8,     // 4-bit port for 16 possible ports
    bitPrio = 16,     // 8-bit priority
    bitData = 32     // 16-bit data
  )
  
  SimConfig.withIVerilog
  .addSimulatorFlag("-g2012") // Enable SystemVerilog 2012 features
  .withFstWave
  .compile(new PifoRTL(testConfig)).doSim { dut =>
    
    
    // Initialize all inputs
    def initializeInputs() = {
      dut.io.popRequest.valid #= false
      dut.io.popRequest.payload.port #= 0
      
      dut.io.push1.valid #= false
      dut.io.push1.payload.port #= 0
      dut.io.push1.payload.priority #= 0
      dut.io.push1.payload.data #= 0
      
      dut.io.push2.valid #= false
      dut.io.push2.payload.port #= 0
      dut.io.push2.payload.priority #= 0
      dut.io.push2.payload.data #= 0
    }
    
    // Helper function to push data into PIFO
    def pushData(port: Int, priority: Int, data: Int, pushInterface: Int = 1) = {
      if (pushInterface == 1) {
        dut.io.push1.valid #= true
        dut.io.push1.payload.port #= port
        dut.io.push1.payload.priority #= priority
        dut.io.push1.payload.data #= data
      } else {
        dut.io.push2.valid #= true
        dut.io.push2.payload.port #= port
        dut.io.push2.payload.priority #= priority
        dut.io.push2.payload.data #= data
      }
      dut.clockDomain.waitRisingEdge()
      
      // Clear push signals
      dut.io.push1.valid #= false
      dut.io.push2.valid #= false
    }
    
    // Helper function to pop data from PIFO
    def popData(port: Int): (Boolean, Long, Long) = {
      dut.io.popRequest.valid #= true
      dut.io.popRequest.payload.port #= port
      dut.clockDomain.waitRisingEdge()
      
      // Clear pop request
      dut.io.popRequest.valid #= false
      dut.clockDomain.waitRisingEdge()
      
      // Check response
      val responseValid = dut.io.popResponse.exist.toBoolean
      val priority = if (responseValid) dut.io.popResponse.payload.priority.toLong else 0
      val data = if (responseValid) dut.io.popResponse.payload.data.toLong else 0
      
      (responseValid, priority, data)
    }
    
    // Reset and initialize
    initializeInputs()
    dut.clockDomain.assertReset()
    fork {
      while (true) {
        sleep(5)
        dut.clockDomain.clockToggle()
        sleep(5)
        dut.clockDomain.clockToggle()
      }
    }
    dut.clockDomain.waitRisingEdge(1024)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitRisingEdge(10)
    
    println("=== BasicPifo Simulation Started ===")

    // Test 1: Basic push and pop operations
    println("\n--- Test 1: Basic Push/Pop Operations ---")

    val pushPort = 2;
    
    // Push some data with different priorities to port 0
    println("Pushing data to port 0:")
    pushData(port = pushPort, priority = 10, data = 0x1001, pushInterface = 1)
    println(s"  Pushed: port=$pushPort, priority=10, data=0x1001")

    pushData(port = pushPort, priority = 5, data = 0x1002, pushInterface = 1)
    println(s"  Pushed: port=$pushPort, priority=5, data=0x1002")

    pushData(port = pushPort, priority = 15, data = 0x1003, pushInterface = 1)
    println(s"  Pushed: port=$pushPort, priority=15, data=0x1003")

    pushData(port = pushPort, priority = 20, data = 0x1004, pushInterface = 1)
    println(s"  Pushed: port=$pushPort, priority=20, data=0x1004")

    // Wait a few cycles for data to settle
    dut.clockDomain.waitRisingEdge(10)
    
    // Pop data from port 0 (should come out in priority order: 15, 10, 5)
    println("Popping data from port 0:")
    for (i <- 0 until 10) {
      val (valid, priority, data) = popData(port = pushPort)
      if (valid) {
        println(s"  Popped: valid=$valid, priority=$priority, data=0x${data.toHexString}")
      } else {
        println(s"  Pop failed: no valid data")
      }
      dut.clockDomain.waitRisingEdge(2)
    }
    
    // Test 2: Multi-port operations
    println("\n--- Test 2: Multi-Port Operations ---")
    
    // Push data to different ports
    println("Pushing data to different ports:")
    pushData(port = 1, priority = 20, data = 0x2001, pushInterface = 1)
    println(s"  Pushed to port 1: priority=20, data=0x2001")
    
    pushData(port = 2, priority = 25, data = 0x2002, pushInterface = 2)
    println(s"  Pushed to port 2: priority=25, data=0x2002")
    
    pushData(port = 1, priority = 18, data = 0x2003, pushInterface = 1)
    println(s"  Pushed to port 1: priority=18, data=0x2003")
    
    dut.clockDomain.waitRisingEdge(3)
    
    // Pop from different ports
    println("Popping from port 1:")
    for (i <- 0 until 2) {
      val (valid, priority, data) = popData(port = 1)
      if (valid) {
        println(s"  Popped from port 1: priority=$priority, data=0x${data.toHexString}")
      } else {
        println(s"  No data in port 1")
      }
      dut.clockDomain.waitRisingEdge(2)
    }
    
    println("Popping from port 2:")
    val (valid2, priority2, data2) = popData(port = 2)
    if (valid2) {
      println(s"  Popped from port 2: priority=$priority2, data=0x${data2.toHexString}")
    } else {
      println(s"  No data in port 2")
    }
    
    // Test 3: Empty PIFO pop attempts
    println("\n--- Test 3: Empty PIFO Pop Attempts ---")
    println("Attempting to pop from empty ports:")
    
    for (port <- Seq(3, 4, 5)) {
      val (valid, priority, data) = popData(port = port)
      println(s"  Port $port: valid=$valid")
    }
    
    // Test 4: Stress test with random data
    println("\n--- Test 4: Stress Test ---")
    
    val random = new scala.util.Random(42) // Fixed seed for reproducibility
    
    // Push random data
    println("Pushing random data:")
    for (i <- 0 until 10) {
      val port = random.nextInt(4)
      val priority = random.nextInt(256)
      val data = random.nextInt(65536)
      val pushInterface = if (random.nextBoolean()) 1 else 2
      
      pushData(port, priority, data, pushInterface)
      println(s"  Pushed: port=$port, priority=$priority, data=0x${data.toHexString}")
    }
    
    dut.clockDomain.waitRisingEdge(5)
    
    // Pop data from all ports
    println("Popping from all ports:")
    for (port <- 0 until 4) {
      println(s"Port $port:")
      var attempts = 0
      var foundData = true
      while (foundData && attempts < 5) {
        val (valid, priority, data) = popData(port)
        if (valid) {
          println(s"  Popped: priority=$priority, data=0x${data.toHexString}")
          attempts += 1
        } else {
          foundData = false
          println(s"  No more data")
        }
        dut.clockDomain.waitRisingEdge(1)
      }
    }
    
    // Final cleanup
    dut.clockDomain.waitRisingEdge(10)
    
    println("\n=== BasicPifo Simulation Completed Successfully ===")
  }
}