package uniquewords
//import scalatags.Text.all._
import scala.io.Source
import sys.process._
import scala.reflect.ClassTag
import scala.xml.XML

object CliApp {
  def linesWithCodec(filename: String, codec: String): Option[Seq[String]] = {
    var result: Option[Seq[String]] = None
    try {
      result = Some(Source.fromFile(filename)(codec).getLines.toVector)
    } catch {
      case _ : Throwable => println("Error parsing: " + filename + ", codec: " + codec)
    }
    result
  }
  def fileLines(filename: String) = {
    var lines = linesWithCodec(filename, "UTF-8")
    if (lines == None) lines = linesWithCodec(filename, "ISO-8859-1")
    if (lines == None) println("Parsing failed.")
    lines.getOrElse(Vector())
  }

  val alphabet = (('a' to 'z') ++ Vector('ä', 'ö', 'å')).toSet
  def fileWords(files: Seq[String]) = {
    files.flatMap(file =>
      fileLines(file)
      .flatMap(line =>
        line
        .split(" ")
        .map(word => word.toLowerCase())
        .filter(word => word.toSet ++ alphabet == alphabet)
        .filter(word => word.length >= 1)
        .toVector
      )
    )
  }

  def wordMap(words: Seq[String]) = {
    words
    .groupBy(identity)
    .mapValues(_.size)
    .toMap
  }

  def main(args: Array[String]) {
    def isSwitch(s : String) = (s(0) == '-')
    def getPlainOptions = args.span(!isSwitch(_))._1
    def getOptionList[T:ClassTag](flag: String, lambda: String => T) = {
      val afterDesiredFlag = args.span(_ != flag)._2.drop(1)
      val beforeNextFlag = afterDesiredFlag.span(!isSwitch(_))._1
      beforeNextFlag.map(lambda(_)).toList
    }
    def getOptions(flag: String): List[String] = getOptionList(flag, identity)
    
    val inFiles = getPlainOptions
    println("Infiles " + inFiles.toList)
    val number = getOptions("--n").map(_.toInt).headOption.getOrElse(20)
    val inWords = fileWords(inFiles)
    println("Words in total: " + inWords.length)
    val frequencies = wordMap(inWords)
    val top2000 = frequencies
                  .toVector
                  .sortBy(wd => -wd._2)
                  .take(number)
    for ((word,count) <- top2000) {
      println(word + " " + count)
    }
  }
}
