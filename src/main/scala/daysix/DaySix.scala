package daysix

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DaySix extends IOApp.Simple {

    override def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DaySix.txt")
        answersA = from(lines).map(group => group.flatMap(_.toCharArray.toList).toSet.size)
        _ <- IO.println(s"sum of answers is: ${answersA.sum}")
        answersB = everyone(from(lines))
        _ <- IO.println(s"sum of answers B is: $answersB")
    } yield ()
 
    
    def everyone(lines: List[List[String]]) = 
        lines.map(group => group.map(_.toCharArray.toSet))
        .map(sets => (sets.reduceLeft(_ `intersect` _)).size)
        .sum
    

        // LOL effffff
    def from(lines: List[String]) = {
        val (thing, last) = lines.foldLeft((List.empty[List[String]], List.empty[String])) {
            case ((outer, inner), xs) => 
                if (xs.isEmpty) 
                    (outer :+ inner, Nil)
                else 
                    (outer, inner :+ xs)
        }
        (thing :+ last)
    }   

}
