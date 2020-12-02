import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 

object DayTwoB extends IOApp.Simple {

    def run: IO[Unit] = {
        // val good = "1-3 a: abcde"
        // val bad = "1-3 b: cdefg"
        for {
            lines <- SimpleFileReader.read("./src/main/resources/DayTwo.txt")
            // lines <- IO.pure(List(good, bad))
            result = ParserB.countValid(lines)
            _ <- IO.println(s"there are: $result valid inputs")
        } yield ()
    }
}


object ParserB {

    def split(s: String): Option[(PolicyB, String)] = 
        s.split(" ").toList match {
            case range :: c :: input :: Nil =>
                val policy = for {
                    parsedC <-Try(c.toCharArray.head).toOption
                    pos <- charPos(range)
                } yield PolicyB(pos, parsedC)
                policy.map(p => (p, input))
            case _ => None 
        }

    def charPos(s: String): Option[Positions] = {
        Try {
            val rangeComponents = s.split("-")
            Positions(rangeComponents(0).toInt, rangeComponents(1).toInt)
        }.toOption
    }

    def countValid(cases: List[String]): Int = 
        cases.foldLeft(0)((acc, input) => 
            ParserB.split(input).map {
                (policy, in) => 
                    if(MatchB.valid(policy.run(in))) acc + 1 else acc 
            }.getOrElse(acc)
        )
}

enum MatchB {
    case Zero
    case Once
    case Over 
}
object MatchB {
    def valid(in: Option[MatchB]): Boolean = 
        in.map {
            case Once => true
            case _=> false 
        }.getOrElse(false)
}

case class Positions(pos1: Int, pos2: Int) {
    def met(cursor: Int): Boolean = 
      cursor == pos1 || cursor == pos2
}

case class PolicyB(positions: Positions, c: Char) {
    import MatchB._

    def run(s: String): Option[MatchB] =
       s.toArray.toList.foldM((1, Zero)) { // start index at 1, trick in question
            case ((pos, matchCase), elem) =>
                check(elem, pos, matchCase) match {
                    case Over => None
                    case rest => Some((pos + 1, rest))
                } 

        }.map(_._2)

    def check(test: Char, position: Int, seen: MatchB): MatchB = {
        if (test != c) seen
        else (positions.met(position), seen) match {
            case (true, Zero) => Once 
            case (true, Once) => Over
            case _ => seen 
        }
    }
}

