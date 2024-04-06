package uniquewords
//import scalatags.Text.all._
import scala.io.Source
import sys.process._
import scala.reflect.ClassTag

object CliApp {
  val alphabet = (('a' to 'z') ++ Vector('ä', 'ö', 'å')).toSet

  def reduceSumMaps(maps: Seq[Map[String, Int]]) = {
    def sumMaps(m1: Map[String, Int], m2: Map[String, Int]) = {
      val merged = m1.toSeq ++ m2.toSeq
      val grouped = merged.groupBy(_._1)
      grouped.mapValues(_.map(_._2).toList.sum).toMap
    }
    maps.reduceLeft( (x,y) => sumMaps(x,y))
  }

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
    if (lines == None) linesWithCodec(filename, "ISO-8859-1")
    val outlines = lines.getOrElse(Vector())
    if (lines == None) {
      println("Parsing failed.")
    } else {
      println("Parsing successful. Lines read: " + outlines.length)
    }
    outlines
  }
  def wordMap(words: Seq[String]) = {
    words
    .groupBy(identity)
    .mapValues(_.size)
    .toMap
  }
  def fileFreqMap(filename: String) = {
    val lines = fileLines(filename)
      .flatMap(line =>
        line
        .split(" ")
        .map(word => word.toLowerCase())
        .filter(word => word.toSet ++ alphabet == alphabet)
        .filter(word => word.length >= 1)
        .toVector
      )
    wordMap(lines)
  }
  def fileWords(files: Seq[String]) = {
    val maps = files.map(file =>
      fileFreqMap(file)
    )
    reduceSumMaps(maps)
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
    println("Infiles: " + inFiles.reduceLeft(_ + "\n" + _))
    val number = getOptions("--n").map(_.toInt).headOption.getOrElse(20)
    val frequencies = fileWords(inFiles)
    val top2000 = frequencies
                  .toVector
                  .sortBy(wd => -wd._2)
                  .take(number)
    val top20formatted = formatToJson(top2000.toMap, 4)
    println(top20formatted)
  }
}
