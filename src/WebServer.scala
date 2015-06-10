import java.net._
import java.io.{InputStreamReader, BufferedReader}
import util.CoinWrangler

import scala.collection.mutable.ListBuffer

object WebServer {
  val defaultPort = 8080
  def main(args: Array[String]) {
    val port = try {args(0).toInt}
      catch {case _: Throwable => defaultPort}

    server(port)
  }

  def server(port: Int) {
    server(new ServerSocket(port))
  }

  def server(ss: ServerSocket) {
    while(true) new HTTPConnection(ss.accept)
  }
}

class HTTPConnection(s: Socket) extends Thread {

  val CONTENTTYPE = "ContentType"
  val GET = "GET"

  start() // von Thread

  override def run() {
    respond(s)
  }

  def respond(s: Socket) {
    try {
      val br = new BufferedReader(new InputStreamReader(s.getInputStream))
      val buf:ListBuffer[String] = ListBuffer()
      var line:String = ""
      while ({ line = br.readLine() ; line != "" } ) {
        buf += line
      }
      val params:Map[String,String] = extractRequest(buf.toList)
      val coins = params.getOrElse("coins", "10,5,2,1").split(",").toList.map(x => x.toInt)
      val amount = params.getOrElse("amount", "10").toInt
      val result = params.getOrElse("path", "coins") match {
        case "coins" => calcCoinDenoms(coins, amount)
      }
      val contenttype = params.getOrElse(CONTENTTYPE, "text/plain")
      s.getOutputStream.write(response(result.toString).getBytes)
    }
    finally {
      s.close()
    }
  }

  def calcCoinDenoms(coins:List[Int], amount:Int):Int = {
    CoinWrangler.calcCoinDenoms(coins, amount)
  }

  def response(html: String, c_type:String = "text/plain", code: String = "200 OK"): String = {
    "HTTP/1.0 " + code + "\n" + CONTENTTYPE + ": " + c_type + "\n" +
    "ContentLength: " + html.length + "\n" + html + "\n"
  }

  def extractRequest(lines: List[String]):Map[String,String] = {
    val maps:List[Map[String,String]] = lines.map(l => processLine(l))
    maps.fold(Map()) { (prev,curr) =>
      prev ++ curr
    }
  }

  def processLine(line:String):Map[String,String] = line match {
    case l if l startsWith "GET" => getParametersFromGET(l substring 4)
    case l if l startsWith CONTENTTYPE => Map(CONTENTTYPE -> l.substring(CONTENTTYPE.length + 2))
  }

  def getParametersFromGET(path:String):Map[String,String] = {
    if(path.charAt(0) == '/' && path.length > 1) {
      val rest = path substring 1
      if(rest contains "?") {
          val path = rest.substring(0, rest.indexOf("?"))
          val params:List[String] = rest.substring(rest.indexOf("?") + 1).split("&").toList
          val paramsToMap:Map[String,String] = params.map(param => param.split("=")(0) -> param.split("=")(1)).toMap
          paramsToMap + ("path" -> path)

      }
      else {
        Map("path" -> rest)
      }
    }
    else Map()
  }
}