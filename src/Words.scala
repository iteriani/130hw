import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import scala.io.Source;
import Lines._
import scala.util.matching.Regex

object Words {

  def apply(file: String) : Iterator[String] =
    Source.fromFile("src\\" + file).getLines.map(_.toLowerCase()).toIterator;

  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] =
    xs.map(f).foldLeft(HashMap[B, Int]())((countmap, word) => countmap + (word -> (countmap.getOrElse(word, 0)+1)))

  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" }

  def sizeFreq(file: String): HashMap[Int, Int] =
    groupFreq[String,Int](apply(file), (x => x.length))

  def charFreq(file: String): HashMap[Char, Int] =
  {
    val chars   =  Source.fromFile("src\\" + file).mkString.replace("\n", "").toIterator
    groupFreq[Char, Char](chars, _.toLower)
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] =
    apply(file).toList.filter(_.length()==size).toIterator

  def wordsWithAllVowels(file: String): Iterator[String] =
    apply(file).toList.map(_.toLowerCase()).filter(vowelCheck).toIterator

  def wordsWithNoVowels(file: String): Iterator[String] =
    apply(file).toList.map(_.toLowerCase()).filter(noVowelCheck).toIterator

  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] ={
    apply(file).toList.map(_.toLowerCase()).filter(x => (re findAllIn(x)).size==1).toIterator
  }

  def vowelCheck(x: String):Boolean =
      x.indexOf('a')>= 0 &&
      x.indexOf('e')>= 0 &&
      x.indexOf('i')>= 0 &&
      x.indexOf('o')>= 0 &&
      x.indexOf("u")>= 0

  def noVowelCheck(x: String):Boolean =
      x.indexOf('a') <  0 &&
      x.indexOf('e')< 0 &&
      x.indexOf('i')< 0 &&
      x.indexOf('o')< 0 &&
      x.indexOf("u")< 0
}

// vim: set ts=2 sw=2 et:

