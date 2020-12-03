package daythree

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DayThree extends IOApp.Simple {

    def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DayThree.txt")
        width = lines.head.length
        depth = lines.length - 1
        finders = lines.toVector.map(line => TreeFinder.from(line, width))
        resultA = TreeFinder.findTrees(finders, depth, List((3, 1)))
        resultB = TreeFinder.findTrees(finders, depth, slopesB)
        _ <- IO.println(s"Part A: encountered $resultA trees for right 3 down 1")
        _ <- IO.println(s"Part B: encountered $resultB")

    } yield ()
  
    val slopesB = List(
        (1, 1), 
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2)
    )

    enum TreeFinder {
        case Treeless
        case Trees(positions: Set[Int], width: Int)

        def isTreeAt(x: Int): Boolean = this match {
            case Treeless => false 
            case Trees(positions, width) => {
                val multiples = Math.floorDiv(x -1 , width)
                positions.contains(x - (multiples * width))
            }
        }
    }


    object TreeFinder {

        def from(input: String, width: Int): TreeFinder = {
            val treePos = input.toCharArray.zipWithIndex.foldLeft(Set.empty[Int]) {
                 case (set, (c, pos)) => if (c == '#') set + (pos + 1) else set
            }
            if(treePos.nonEmpty) Trees(treePos, width) else Treeless
        }

        def findTrees(finders: Vector[TreeFinder], depth: Int, slopes: List[(Int, Int)]): Long = {
            val result = slopes.map {
                case (right, down) => mapPositions(right, down, depth).foldLeft(0) {
                    case (acc, (x, y)) => if (finders(y).isTreeAt(x)) acc + 1 else acc
                }
            }
            result.map(_.toLong).product // gotta look out for that integer overflow
        }
    }

    def mapPositions(right: Int, down: Int, depth: Int): List[(Int, Int)] =
        List.unfold((1,0)) {
            case (x, y) => {
                val newX = x + right
                val newY = y + down 
                if (newY > depth) None 
                else {
                    val elem = (newX, newY) 
                    Some((elem, elem))
                }
            }
        }
    
}
