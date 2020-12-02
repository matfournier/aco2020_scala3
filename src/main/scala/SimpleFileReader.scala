import cats.effect.IO
import scala.io.Source

object SimpleFileReader {
    def read(fname: String): IO[List[String]] = 
        IO(Source.fromFile(fname).getLines.toList)
}