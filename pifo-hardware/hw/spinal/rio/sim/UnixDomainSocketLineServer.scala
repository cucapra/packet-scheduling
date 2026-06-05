package rio.sim

import java.io.{BufferedReader, InputStreamReader}
import java.net.SocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import spinal.core.sim._
import spinal.sim.SimThread

object UnixDomainSocketLineServer {
  case class KeyValueLine(fields: Map[String, String]) {
    def requireString(key: String): String = {
      fields.getOrElse(key, throw new IllegalArgumentException(s"missing $key"))
    }

    def requireInt(key: String): Int = {
      parseInt(requireString(key))
    }
  }

  def start(socketPath: String, label: String = "UnixDomainSocket")(onLine: String => Unit): SimThread = {
    val socket = Paths.get(socketPath)
    try {
      val server = openUnixDomainServer(socket)
      println(s"[$label] Listening on $socketPath")

      fork {
        try {
          while (true) {
            val client = server.accept()
            if (client == null) {
              sleep(1000)
            } else {
              handleClient(client, onLine)
            }
          }
        } finally {
          server.close()
          Files.deleteIfExists(socket)
        }
      }
    } catch {
      case e: UnsupportedOperationException =>
        startNetcatFallback(socket, label, e)(onLine)
    }
  }

  def startKeyValue(
    socketPath: String,
    label: String = "UnixDomainSocket"
  )(onLine: KeyValueLine => Unit): SimThread = {
    start(socketPath, label) { line =>
      try {
        parseKeyValueLine(line).foreach(onLine)
      } catch {
        case e: IllegalArgumentException =>
          println(s"[$label] Ignoring '$line': ${e.getMessage}")
      }
    }
  }

  def parseKeyValueLine(line: String): Option[KeyValueLine] = {
    val commandText = line.takeWhile(_ != '#').trim
    if (commandText.isEmpty) {
      return None
    }

    val pairs = commandText.split("\\s+").filter(_.nonEmpty).map { token =>
      val separator = token.indexOf("=")
      if (separator <= 0 || separator == token.length - 1) {
        throw new IllegalArgumentException(s"expected key=value token, got '$token'")
      }
      token.substring(0, separator) -> token.substring(separator + 1)
    }
    val duplicateKeys = pairs.groupBy(_._1).collect { case (key, values) if values.length > 1 => key }
    if (duplicateKeys.nonEmpty) {
      throw new IllegalArgumentException(s"duplicate keys: ${duplicateKeys.mkString(", ")}")
    }

    Some(KeyValueLine(pairs.toMap))
  }

  def parseInt(value: String): Int = {
    java.lang.Long.decode(value).intValue()
  }

  private def startNetcatFallback(
    socket: Path,
    label: String,
    cause: Throwable
  )(onLine: String => Unit): SimThread = {
    ensureFreshSocketPath(socket)

    val process = new ProcessBuilder("nc", "-Ulk", socket.toString)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
    val reader = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
    val lineBuffer = new StringBuilder
    println(s"[$label] Listening on $socket via nc fallback (${cause.getMessage})")

    fork {
      try {
        while (process.isAlive) {
          var consumed = false
          while (reader.ready()) {
            val next = reader.read()
            if (next == '\n') {
              onLine(lineBuffer.toString)
              lineBuffer.clear()
            } else if (next >= 0 && next != '\r') {
              lineBuffer.append(next.toChar)
            }
            consumed = true
          }

          if (!consumed) {
            sleep(1000)
          }
        }

        val tail = lineBuffer.toString.trim
        if (tail.nonEmpty) {
          onLine(tail)
        }
        println(s"[$label] nc listener exited with code ${process.waitFor()}")
      } finally {
        process.destroy()
        reader.close()
        Files.deleteIfExists(socket)
      }
    }
  }

  private def openUnixDomainServer(socket: Path): ServerSocketChannel = {
    ensureFreshSocketPath(socket)

    try {
      val protocolFamilyClass = Class.forName("java.net.ProtocolFamily")
      val standardProtocolFamilyClass = Class.forName("java.net.StandardProtocolFamily")
      val unixProtocol = standardProtocolFamilyClass
        .getMethod("valueOf", classOf[String])
        .invoke(null, "UNIX")
      val addressClass = Class.forName("java.net.UnixDomainSocketAddress")
      val address = addressClass
        .getMethod("of", classOf[Path])
        .invoke(null, socket)
        .asInstanceOf[SocketAddress]

      val server = classOf[ServerSocketChannel]
        .getMethod("open", protocolFamilyClass)
        .invoke(null, unixProtocol)
        .asInstanceOf[ServerSocketChannel]
      server.bind(address)
      server.configureBlocking(false)
      server
    } catch {
      case e: ClassNotFoundException =>
        throw new UnsupportedOperationException(
          "Unix domain sockets require a JDK with java.net.UnixDomainSocketAddress",
          e
        )
    }
  }

  private def ensureFreshSocketPath(socket: Path): Unit = {
    if (socket.getParent != null) {
      Files.createDirectories(socket.getParent)
    }
    Files.deleteIfExists(socket)
  }

  private def handleClient(client: SocketChannel, onLine: String => Unit): Unit = {
    val buffer = ByteBuffer.allocate(4096)
    val lineBuffer = new StringBuilder
    client.configureBlocking(false)

    try {
      var connected = true
      while (connected) {
        buffer.clear()
        val bytesRead = client.read(buffer)
        if (bytesRead > 0) {
          buffer.flip()
          lineBuffer.append(StandardCharsets.UTF_8.decode(buffer).toString)
          drainLines(lineBuffer, onLine)
        } else if (bytesRead < 0) {
          connected = false
        } else {
          sleep(1000)
        }
      }

      val tail = lineBuffer.toString.trim
      if (tail.nonEmpty) {
        onLine(tail)
      }
    } finally {
      client.close()
    }
  }

  private def drainLines(lineBuffer: StringBuilder, onLine: String => Unit): Unit = {
    var newline = lineBuffer.indexOf("\n")
    while (newline >= 0) {
      val line = lineBuffer.substring(0, newline).stripSuffix("\r")
      lineBuffer.delete(0, newline + 1)
      onLine(line)
      newline = lineBuffer.indexOf("\n")
    }
  }
}
