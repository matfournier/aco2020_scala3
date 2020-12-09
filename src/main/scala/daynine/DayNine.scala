package daynine

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DayNine extends IOApp.Simple {
  
    override def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DayNine.txt")
        distance =  25 //5
        (preamble, search) = lines.map(_.toLong).splitAt(distance)
        preambles = lines.map(_.toLong).sliding(distance).toVector
        answerA = search.zipWithIndex.find {
            case (s, i) => {
                val lookUp = Brute.lookupMap(preambles(i))
                !lookUp.keys.toSet.contains(s)
            }
        }.map(_._1)
        _ <- IO.println(s"answerA: $answerA")
        answerB = Brute.findFirstContinuous(lines.map(_.toLong), 70639851)
        _ <- IO.println(s"answerB: $answerB")
    } yield ()

    object Brute {

        // this elaborate lookup map did not turn out to be useful for part :( )
        def lookupMap(pre: List[Long]): Map[Long, Set[(Long, Long)]] = {
            pre.combinations(2).toList.map {
                case p1 :: p2 :: nil => (p1, p2)
            }
            .map {
                case (p1, p2) => (p1 + p2) -> (p1, p2)
            }.groupBy(_._1).view.mapValues(a => {
               val tuples =  a.map(_._2)
               val flipped = tuples.map {
                   case (a, b) => (b, a)
               }
               (tuples ++ flipped).toSet
            }).toMap
        }

        def findFirstContinuous(xs: List[Long], target: Long): Long = {
            val cont = findContinous(xs, target, Nil)
            if (cont.nonEmpty) cont.min + cont.max else findFirstContinuous(xs.drop(1), target)
        }

        def findContinous(xs: List[Long], target: Long, acc: List[Long]): List[Long] = {
            if (acc.sum == target) acc
            else if (acc.sum > target) List()
            else findContinous(xs.drop(1), target, xs.head +: acc)
        }


    }
}
