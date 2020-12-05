package dayfive

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DayFive extends IOApp.Simple {
    
    override def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DayFive.txt")
        seats = lines.map(parseSeat).map(_.seatId).flatten
        maxId = seats.max 
        _ <- IO.println(s"max seatId: $maxId")
        _ <- IO.println(s"missing seat: ${findMissingSeat(seats)}")
    } yield ()

    type Pair = (Int, Int)

    case class Seat(s: String, row: Pair, col: Pair) {
        def seatId: Option[Int] = {
            if (row._1 == row._2 && col._1 == col._2) Some((row._1 * 8) + col._1)
            else None
        }
            
    }

    // lower
    val applyFL: Pair => Pair = {
        case (lower, upper) => 
            val half = (upper - lower) / 2
            (lower, lower + half)
    }

    // upper
    val applyBR: Pair => Pair = {
        case (lower, upper) =>
             val half = (upper - lower) / 2
             (upper - half, upper)

    }
        
    def applyParse(p: Pair, f: Pair => Pair): Pair =
        f(p)

            
    def parseSeat(s: String): Seat = {
        val init: Seat = Seat(s, (0, 127), (0, 7))

        s.toList.foldLeft(init)((seat, c) => c match {
            case 'F' => seat.copy(row = applyParse(seat.row, applyFL))
            case 'B' => seat.copy(row = applyParse(seat.row, applyBR))
            case 'R' => seat.copy(col = applyParse(seat.col, applyBR))
            case 'L' => seat.copy(col = applyParse(seat.col, applyFL))
        }
        )
    }

    def findMissingSeat(s: List[Int]): Int = 
        Range(s.min, s.max).inclusive.toSet.diff(s.toSet).toList.head


  
}
