import cats.effect.{IO, IOApp , Resource}
import cats.data._
import cats.implicits._
import java.io._
import scala.io.Source
import scala.annotation.tailrec
import util.SimpleFileReader

object DayOne extends IOApp.Simple {

    def run: IO[Unit] = {
        for {
            lines <- SimpleFileReader.read("./src/main/resources/DayOne.txt")
            result = findTwo(lines.map(_.toInt)).map(_ * _)
            resultThree = findThree(lines.map(_.toInt)).map(_.foldLeft(1)(_ * _))
            _ <- IO.println(s"resultTwo: $result\nresultThree: $resultThree")
        } yield ()
    }

    def findTwo(lines: List[Int]): Option[(Int, Int)] = {    
        @tailrec
        def go(input: List[Int]): Option[(Int, Int)] = {
            input match {
                case x :: xs => xs.find(v => (x + v) == 2020) match {
                    case Some(v) => Some(x, v)
                    case None => go(input.drop(1))
                }
                case _ => None
            }
        }

    
        go(lines)
    }

    def findThree(lines: List[Int]): Option[List[Int]] =  {
        @tailrec
        def go(input: List[Int], v1: Int): Option[List[Int]] = 
            input match {
                case x :: xs => xs.find(x2 => (x + x2 + v1) == 2020) match {
                    case Some(v) => List(x, v, v1).some
                    case None => go(input.drop(1), v1)
            }
                case _ => None
            }
        
        val empty: (Option[List[Int]], List[Int]) = (None, lines)
        val (result, _) = lines.foldLeft(empty) {
            case ((found, inputs), b) => found match {
                case Some(r) => (Some(r), Nil)
                case None => go(inputs, b) match {
                    case Some(r) => (Some(r), Nil)
                    case None => (found, inputs.drop(1))
                }
            }
        }
        result
    }
}


