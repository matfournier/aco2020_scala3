package dayten

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DayTen extends IOApp.Simple {

    override def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DayTen.txt")
        intLines = lines.map(_.toInt)
        a = answerA(intLines)
        _ <- IO.println(s"answerA: $a")
        b = goAnswerB((0 +: intLines).sorted)
        _ <- IO.println(s"answerB: $b")
    } yield ()
 
    
    def answerA(xs: List[Int]): Int = {
        val pairs: (Int, Int) = xs.sorted.sliding(2).toList.map {
            case h :: t :: nil => t - h
        }.foldLeft((1, 1)) {
            case ((ones, threes), e ) => 
                if (e == 1) ((ones + 1), threes)
                else if (e == 3) ((ones, threes + 1))
                else ((ones, threes))
        }
        println(pairs)
        pairs._1 * pairs._2
    }

    // WAAAAY overthought this one, with false paths and this weird recursive descent thing
    // drew it out on paper and it was easier 
    
    def goAnswerB(xs: List[Int]): Long = {
    val init = Map(xs.max + 3 -> 1L) // overflow again
    val counts = xs.reverse.foldLeft(init) {
        case (counts, next) =>
        println(s"counts: $counts, next: $next")
      val count = (next + 1 to next + 3).map(counts.getOrElse(_, 0L)).sum
      println(s"-------count: $count")
      counts + (next -> count)
    }
    println(counts)
    counts(0)
  }

}
