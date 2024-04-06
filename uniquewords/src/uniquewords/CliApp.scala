package uniquewords
//import scalatags.Text.all._
import scala.io.Source
import sys.process._
import scala.reflect.ClassTag

object CliApp {
  val alphabet = (('a' to 'z') ++ Vector('ä', 'ö', 'å')).toSet
  def fileWords(files: Seq[String]) = {
    files.flatMap(file =>
      Source
      .fromFile(file)
      .getLines
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

  def formatToJson(wordMap: Map[String, Int], indentLevel: Int) = {
    val indent = " " * indentLevel
    val sortedElems =  wordMap.toVector.sortBy(-_._2)
    val formattedElems = sortedElems.map(pair => "\"" + pair._1 + "\": " + pair._2)
    val elemsJson = indent + formattedElems.reduceLeft(_ + ",\n" + indent + _)
    "{\n" + elemsJson + "\n}"
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
    //val outFiles = getOptions("--out"))

    val inWords = fileWords(inFiles)
    println("Words in total: " + inWords.length)
    val frequencies = wordMap(inWords)
    val top2000 = frequencies
                  .toVector
                  .sortBy(wd => -wd._2)
                  .take(20)
    for ((word,count) <- top2000) {
      println(word + " " + count)
    }
    val top20formatted = formatToJson(top2000.toMap, 4)
    println(top20formatted)
  }
}
