package rio

import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

/**
  * ControllerFactory allows registering multiple dispatch handlers each with their own
  * target type. Each addDispatch returns a Stream[To] which the caller can wire to
  * a memory/port. Call build(controlStream) once to wire all registered handlers.
  */
class ControllerFactory(config: EngineConfig) {
  // Each handler is represented as a function that, given a forked control head Stream,
  // will wire it to the previously-created target Stream.
  private val handlers = ArrayBuffer.empty[Stream[ControlMessage] => Unit]

  /**
    * Register a dispatch with a hardware predicate `cond` on the control head Stream.
    * `prototype` is used to create the target Stream type for wiring. `assign` maps
    * fields from the incoming ControlMessage into the target payload.
    *
    * Example:
    *   val s = factory.addDispatch(_.payload.command === ControlCommand.MODIFY_MAPPING, MapperUpdater(...)) { (to, from) =>
    *     to.inputId := from.flowId
    *     to.outputId := from.vPifoId
    *   }
    */
  def dispatch[To <: Data](command : SpinalEnumElement[ControlCommand.type], target: Flow[To])(assign: (To, ControlMessage) => Unit) = {
    // capture s and assign in the handler function; when build() is called we will
    // pass a forked head Stream to this function to perform the translateInto wiring.
    handlers += { (head: Stream[ControlMessage]) =>
      target << head
        .takeWhen(head.payload.command === command)
        .translateInto(Stream(target.payload)) { (to: To, from: ControlMessage) =>
          assign(to, from)
        }.toFlow
    }
  }

  /**
    * Wire all registered handlers to the provided controlStream. Call once after
    * registering handlers.
    */
  def build(controlStream: Stream[ControlMessage]): Unit = {
    if (handlers.isEmpty) return

    val heads = StreamFork(controlStream, handlers.length)
    (heads zip handlers).foreach { case (stream, handler) =>
      handler(stream)
    }
  }
}
