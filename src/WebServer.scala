import java.net.{ServerSocket,Socket}
import java.io.{File, FileNotFoundException, InputStreamReader, BufferedReader}
import scala.io.Source
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

  val WEBROOT = "static"
  val CONTENTTYPE = "Content-Type"
  val CONTENTLENGTH = "Content-Length"
  val GET = "GET"

  start()

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
      val path = params.getOrElse("path", "index.html")

      if(path.contains(".")) {
        val result = getFileContent(path)
        s.getOutputStream.write(response(result.toString, "text/html").getBytes)
      }
      else if(path.contentEquals("coins")) {
        val c = params.getOrElse("coins", "10,5,2,1")
        val coins:List[Int] = c match {
          case c if c.contains(",") => c.split(",").toList.map(x => x.toInt)
          case c if c.contains("%2C") => c.split("%2C").toList.map(x => x.toInt)
          case "euro" => List(500,200,100,50,20,10,5,2,1)
        }
        val amount = params.getOrElse("amount", "100").toInt
        val result = calcCoinDenoms(coins, amount)
        s.getOutputStream.write(response(result.toString).getBytes)
      }
    }
    finally {
      s.close()
    }
  }

  def getFileContent(path:String):String = {
    try {
      val source = Source.fromFile(new File(".").getCanonicalPath + "/" + WEBROOT + "/" + path)
      val content = try source.mkString finally source.close()
      content
    }
    catch {
      case fnfe: FileNotFoundException => s"$path does not exist"
      case e: Exception => "Something went terribly wrong"
    }
  }

  def calcCoinDenoms(coins:List[Int], amount:Int):Int = {
    CoinWrangler.calcCoinDenoms(coins, amount)
  }

  def response(html: String, c_type:String = "text/plain", code: String = "200 OK"): String = {
    "HTTP/1.1 " + code + "\n" +
    "Server: Scala Webserver\n" +
    CONTENTTYPE + ": " + c_type + "\n" +
    CONTENTLENGTH +": " + html.length + "\n\n" +
    html + "\n"
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
    case _ => Map()
  }

  def getParametersFromGET(path:String):Map[String,String] = {
    if(path.charAt(0) == '/' && path.length > 1) {
      val rest = try path.substring(1, path.indexOf(" ")) catch { case e: StringIndexOutOfBoundsException => path.substring(1)}
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