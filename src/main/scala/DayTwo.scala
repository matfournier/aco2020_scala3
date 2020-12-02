import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
object DayTwo extends IOApp.Simple {

    def run: IO[Unit] = {
        // val good = "1-3 a: abcde"
        // val bad = "1-3 b: cdefg"
        for {
            lines <- SimpleFileReader.read("./src/main/resources/DayTwo.txt")
            result = Parser.countValid(lines)
            _ <- IO.println(s"there are: $result valid inputs")
        } yield ()
    }
}

object Parser {

    def split(s: String): Option[(Policy, String)] = 
        s.split(" ").toList match {
            case range :: c :: input :: Nil =>
                val policy = for {
                    parsedC <-Try(c.toCharArray.head).toOption
                    limit <- charLimit(range)
                } yield Policy(limit, parsedC)
                policy.map(p => (p, input))
            case _ => None 
        }

    def charLimit(s: String): Option[Limit] = {
        Try {
            val rangeComponents = s.split("-")
            Limit(rangeComponents(0).toInt, rangeComponents(1).toInt)
        }.toOption
    }

    def countValid(cases: List[String]): Int = 
        cases.foldLeft(0)((acc, input) => 
            Parser.split(input).map {
                (policy, in) => 
                    if(Match.valid(policy.run(in))) acc + 1 else acc 
            }.getOrElse(acc)
        )
}

enum Match {
    case NotApplicable
    case Under 
    case Inside
    case Over 
}
object Match {
    def valid(in: Option[Match]): Boolean = 
        in.map {
            case Inside => true
            case _=> false 
        }.getOrElse(false)
}

case class Limit(min: Int, max: Int) {
    import Match._
    def met(n: Int): Match = 
        if (n < min) Under 
        else if (n >= min && n <= max) Inside
        else Over 
}

case class Policy(limit: Limit, c: Char) {
    import Match._

    def run(s: String): Option[Match]=  {
       s.toArray.toList.foldM((0, NotApplicable)) {
            case ((seen, matchCase), elem) =>
                check(elem, seen) match {
                    case Over => None
                    case NotApplicable => Some((seen, matchCase))
                    case rest => Some((seen +1, rest))
                } 

        }.map(_._2)
    }

    def check(test: Char, seen: Int): Match = 
        if (test != c) NotApplicable
        else limit.met(seen +1)
}
