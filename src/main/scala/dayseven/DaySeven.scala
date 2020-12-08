package dayseven

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DaySeven extends IOApp.Simple {

    override def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DaySeven.txt")
        parsedBags = lines.map(Bag.parse(_)).map(bag => (bag.colour, bag)).toMap
        solutionA = parsedBags.keys.count(b => contains(b, parsedBags))
        _ <- IO.println(solutionA)
        solutionB = count("shiny gold bag", parsedBags)
        _ <- IO.println(solutionB)
    } yield ()
  

    def contains(bag: String, bags: Map[String, Bag]): Boolean = 
        bags(bag).connectedTo.contains("shiny gold bag") || bags(bag).connectedTo.exists {
        case (nextBag, _) => contains(nextBag, bags)
    }

    def count(bag: String, bags: Map[String, Bag]): Int = 
        bags(bag).connectedTo.map {
            case (next, counts) => counts * { 
                val nextCounts = count(next, bags)
                if (nextCounts == 0) 1 else nextCounts + 1
            }
        }.sum


    case class Bag(colour: String, connectedTo: Map[String, Int])

    object Bag {
        def parse(input: String): Bag = {
            val split = input.split("contain").toList
            val colour = split.head.replace("bags", "bag").trim()
            val rest = split.last
            val contents: Map[String, Int] = if(rest.contains("no other")) {
                Map()
            } else {
                rest.split(",")
                .toList.map(_.trim())
                .map(s => {
                    val num = s.head.toString.toInt
                    val desc = s.drop(1).trim().replace("bags", "bag").replace(".", "")
                    (desc, num)
                }).toMap
            }

            Bag(colour, contents)
        }

    }


}
