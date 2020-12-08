package dayeight 

import cats.effect.{IO, IOApp}
import cats._
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader

object DayEight extends IOApp.Simple {

    override def run: IO[Unit] = for {
            lines <- SimpleFileReader.read("./src/main/resources/DayEight.txt")
            parsed = lines.zipWithIndex.map {
                        case (s, i) => Operation.parse(s, i)
                    }
            answerA = Operation.run(parsed)
            answerB = Operation.runB(parsed)
            _ <- IO.println(s"answer A: $answerA")
            _ <- IO.println(s"answer B: $answerB")
    } yield ()


    enum Operation(val i: Int) {
        case Nop(v: Int, override val i: Int) extends Operation(i)
        case Add(v: Int, override val i: Int) extends Operation(i)
        case Jump(rel: Int, override val i: Int)  extends Operation(i)
    }

    object Operation {
        import Operation._

        def parse(s: String, i: Int): Operation = s.split(" ").toList match {
            case (op :: v :: Nil) => op match {
                case "nop" => Nop(v.toInt, i)
                case "acc" => Add(v.toInt, i)
                case "jmp" => Jump(v.toInt, i)
            }
        }

        def run(ops: List[Operation]) = {
            val stack = ops.toVector
            val stream: Stream[Int] = 
                Stream.unfold((0, 0, Set.empty[Operation]))((acc, pos, seen) => {
                    val op = stack(pos)
                    if (seen.contains(op)) None
                    else {
                        op match {
                            case Nop(_, _) => Some(acc -> (acc, pos + 1, seen + op))
                            case a: Add => Some(acc + a.v-> (acc + a.v, pos + 1, seen + op))
                            case j: Jump => Some(acc -> (acc, pos + j.rel, seen + op))
                        }
                    }
            })
            stream.last
        }

        // got tired, brute force 
        def runB(ops: List[Operation]): Int = {
            val sstack = ops.toVector
            val length = sstack.length
            val nopJmpPositions = ops.zipWithIndex.filter {
                case (_: Nop, i) => true
                case (_: Jump, i) => true 
                case _ => false
            }.map(_._2).toList

            val stream: Stream[Int] = 
                Stream.unfold((0, 0, Set.empty[Operation], ops.toVector, nopJmpPositions))((acc, pos, seen, stack, left) => {
                    if (pos > stack.length - 1) {
                        None
                    }
                    else if (seen.contains(stack(pos))) {
                        val newAcc = 0 
                        val newPos = 0 
                        val newSeen = Set.empty[Operation]

                        val swap = sstack(left.head) match {
                            case n:Nop => Jump(n.v, n.i)
                            case j:Jump => Nop(j.rel, j.i)
                            case _ => throw new Exception("can't reach")
                        }
                        val newStack = sstack.updated(swap.i, swap)
                        Some(newAcc, (newAcc, newPos, newSeen, newStack, left.tail))
                    }
                    else {
                        val op = stack(pos)
                        op match {
                            case Nop(_, _) => Some(acc -> (acc, pos + 1, seen + op, stack, left))
                            case a: Add => Some(acc + a.v-> (acc + a.v, pos + 1, seen + op, stack, left))
                            case j: Jump => Some(acc -> (acc, pos + j.rel, seen + op, stack, left))
                        }
                    }
            })
            stream.last
        }

    
           
    }
}
