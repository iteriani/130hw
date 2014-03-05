import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io._
import scala.io.Source

import java.io.PrintWriter

case class Entry ( account   : String
                   , password  : String
                   , uid       : Int
                   , gid       : Int
                   , gecos     : String
                   , directory : String
                   , shell     : String
                   )

object Entry {

  def apply(line: String) : Entry = {
    val lines = line.split(":")
    Entry(lines(0), lines(1), Integer.valueOf(lines(2)), Integer.valueOf(lines(3)), lines(4), lines(5), lines(6))
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    List(w,w.reverse).toIterator
  }

  def transformCapitalize(w: String) : Iterator[String] = {
   if(w == "")
     Iterator("")
   else {
    val list = List(w.head, w.head.toUpper).toIterator
    for(hd <- list; tl <- transformCapitalize(w.tail))
      yield(hd + tl)
   }
  }

  def transformDigits(w:String) : Iterator[String] = {
    if(w == "")
      Iterator("")
    else {
      val list = transform(w.head)
      for(hd <- list; tl <- transformDigits(w.tail))
      yield(hd + tl)
    }
  }

  def transform(w:Char) : Iterator[Char] = {
    w.toLower match{
      case 'o' => Iterator(w, '0')
      case 'z' => Iterator(w, '2')
      case 'a' => Iterator(w, '4')
      case 'b' => Iterator(w, '6', '8')
      case 'g' => Iterator(w, '9')
      case 'q' => Iterator(w, '9')
      case 'i' => Iterator(w, '1')
      case 'l' => Iterator(w, '1')
      case 'e' => Iterator(w,'3')
      case 's' => Iterator(w, '5')
      case 't' => Iterator(w,'7')
      case _ => Iterator(w)

    }
  }

  def checkPassword(plain: String, enc: String) : Boolean =
    Crypt.crypt(enc, plain) == enc

  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    val users = Source.fromFile(pwdFile).getLines().map(Entry.apply)
    val words = Source.fromFile(wordsFile).getLines().toList.sortBy(_.length()).toIterator
    val writer = new PrintWriter(new File(outFile))
    for(word <- words){
      for(user <- users){
        if(crackPW(word,user.password) == true){
          writer.write(user.account + "=" + word)
        }
      }
    }
  }

  def crackPW(word:String, password:String):Boolean = {
    println("working on " + word)
    val wordCheck = transformCapitalize(word).toList ::: transformReverse(word).toList ::: transformDigits(word).toList
    for(enc <- wordCheck.toIterator){
      if(checkPassword(password, enc) == true){
        return true
      }
    }
    return false
  }



  def main(args: Array[String]) = {
    println("Begin: Cracking Passwords")
    apply("src/passwd", "src/words", "soln.1");
    //apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:
