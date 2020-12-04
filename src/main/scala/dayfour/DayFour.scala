package dayfour

import cats.effect.{IO, IOApp}
import cats.data._
import cats.implicits._
import scala.util.Try 
import util.SimpleFileReader
import scala.util.matching.Regex

object DayFour extends IOApp.Simple {

    override def run: IO[Unit] = for {
        lines <- SimpleFileReader.read("./src/main/resources/DayFour.txt")
        passports = Passport.from(lines)
        validA = Filters.validPassportsA(passports)
        validB = Filters.validPassportsB(passports)
        _ <- IO.println(s"valid passports A: ${validA.length}")
        _ <- IO.println(s"valid passports A: ${validB.length}")
        } yield () 
        
    val allKeys: Set[String] = 
        Set(
            "byr",
            "iyr",
            "eyr",
            "hgt",
            "hcl",
            "ecl",
            "pid",
            "cid"
        )
    val allowedMissing: Set[String] = Set("cid")

    case class Passport(kv: List[(String, String)]) {
        val keys: Set[String] = kv.map {
            case (key, _) => key
        }.toSet

        val items: Map[String, String] = kv.toMap
    }
    
    object Filters {
        type Check = Passport => Boolean

        val validEyeColor: Set[String] = Set(
            "amb",
            "blu",
            "brn",
            "gry",
            "grn",
            "hzl",
            "oth"
        )
        val keysPresent: Check = passport =>
            val check = allKeys.diff(passport.keys)
            check.isEmpty || check.diff(allowedMissing).isEmpty

        val birthYear: Check = passport => 
            val year = passport.items("byr")
            year.length == 4 && (year.toInt >= 1920 && year.toInt <= 2002)

        val issueYear: Check = passport => 
            val year = passport.items("iyr")
            year.length == 4 && (year.toInt >= 2010 && year.toInt <= 2020)

        val expireYear: Check = passport => 
            val year = passport.items("eyr")
            year.length == 4 && (year.toInt >= 2020 && year.toInt <= 2030)

        
        val hairCheck: Check = passport =>
            val hcl = passport.items("hcl")
            val regex = new Regex("""^#([a-f0-9]{6}|[a-f0-9]{3})$""")
            regex.findFirstIn(hcl).map(_ => true).getOrElse(false)
            

        val eyeCheck: Check = passport => 
            val ecl = Set(passport.items("ecl"))
            ecl.size == 1 && ecl.intersect(validEyeColor).size == 1

        val pidCheck: Check = passport => 
            val pid = passport.items("pid")
            if (pid.length == 9) Try(pid.toInt).toOption.fold(false)(_ => true)
            else false 

        
        // to lazy to regex
        val heightCheck: Check = passport => {
            val height = passport.items("hgt")
                height.reverse.take(2).reverse match {
                    case "cm" => 
                        val num = height.reverse.drop(2).reverse.toInt
                        num >= 150 && num <= 193
                    case "in" =>
                        val num = height.reverse.drop(2).reverse.toInt
                        num >= 59 && num <= 76
                    case _ => false 
                } 
        }


        val checksA: List[Check] = List(keysPresent)


        val checksB: List[Check] = List(
            keysPresent, // needs to be first because I'm lazy --unsasfe map access
            birthYear,
            issueYear,
            expireYear,
            heightCheck,
            hairCheck,
            eyeCheck,
            pidCheck
            )

        def validPassportsA(passports: List[Passport]): List[Passport] = 
            passports.filter(passport => checksA.forall(check => check(passport)))
        def validPassportsB(passports: List[Passport]): List[Passport] = 
            passports.filter(passport => checksB.forall(check => check(passport)))
    }

    object Passport {
        
        // LOL effffff
        def from(lines: List[String]) = {
            val (passports, last) = lines.foldLeft((List.empty[List[String]], List.empty[String])) {
                case ((outer, inner), xs) => 
                    if (xs.isEmpty) 
                        (outer :+ inner, Nil)
                    else 
                        val splits = xs.split(" ").toList
                        (outer, inner ++ splits)
            }
            (passports :+ last).map(passport => 
                Passport(
                    passport.map(_.split(":").toList).map(pair => (pair.head, pair.last))
                )
            )
        
        }   
    }
}