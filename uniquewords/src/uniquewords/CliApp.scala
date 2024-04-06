package uniquewords
//import scalatags.Text.all._
import scala.io.Source
import sys.process._
import scala.reflect.ClassTag
import scala.collection.mutable.Buffer

object CliApp {
  val alphabet = (('a' to 'z') ++ Vector('ä', 'ö', 'å')).toSet

  def reduceSumMaps(maps: Seq[Map[String, Int]]) = {
    if (maps.length == 0) {
      Map[String, Int]()
    } else {
      def sumMaps(m1: Map[String, Int], m2: Map[String, Int]) = {
        val merged = m1.toSeq ++ m2.toSeq
        val grouped = merged.groupBy(_._1)
        grouped.mapValues(_.map(_._2).toList.sum).toMap
      }
      maps.reduceLeft( (x,y) => sumMaps(x,y))
    }
  }

  def linesWithCodec(filename: String, codec: String): Option[Seq[String]] = {
    var result: Option[Seq[String]] = None
    try {
      result = Some(Source.fromFile(filename)(codec).getLines.toVector)
    } catch {
      case _ : Throwable => Console.err.println("Error parsing: " + filename + ", codec: " + codec)
    }
    result
  }
  def fileLines(filename: String) = {
    var lines = linesWithCodec(filename, "UTF-8")
    if (lines == None) lines = linesWithCodec(filename, "ISO-8859-1")
    val outlines = lines.getOrElse(Vector())
    if (lines == None) {
      Console.err.println("Parsing failed.")
    } else {
      Console.err.println("Parsing successful. Lines read: " + outlines.length)
    }
    outlines
  }
  def getWordCounts(words: Seq[String]) = {
    words
    .groupBy(identity)
    .mapValues(_.size)
    .toMap
  }
  def splitGroupGetFreqs(lines: Seq[String]) = {
    var result = Buffer[Map[String, Int]]()
    val chunkSize = 1000000
    var b = Buffer[String]()
    var currentString = ""
    val totalLength = lines.map(_.length).sum
    var index = 0
    for (line <- lines) {
      for (c <- line.toLowerCase()) {
        if (c == ' ') {
          b.append(currentString)
          if (index % 1000 == 0) {
            Console.err.print("\u001B[A")
            Console.err.println("\rCharacter n.o. " + index + "/" + totalLength)
          }
          currentString = ""
          if (b.length == chunkSize) {
            result.append(getWordCounts(b.toVector))
            b = Buffer[String]()
          }
        } else if (alphabet.contains(c)) {
          currentString += c
        }
        index += 1
      }
      if (currentString.length > 0) {
        b.append(currentString)
        currentString = ""
        if (b.length == chunkSize) {
            result.append(getWordCounts(b.toVector))
            b = Buffer[String]()
        }
      }
    }
    if (b.length > 0) {
      result.append(getWordCounts(b.toVector))
    }
    result.toVector
  }
  def fileFreqMap(filename: String) = {
    Console.err.println("Get lines from file " + filename + " ...")
    val lines = fileLines(filename)
    Console.err.println("Process to get words ...")
    //val words = lines(0).split(' ')
    // Group words to limit memory usage
    Console.err.println("Group words...")
    val frequencyMaps = splitGroupGetFreqs(lines)
    //val groupedWords = words.grouped(100000)
    Console.err.println("Create word count maps...")
    reduceSumMaps(frequencyMaps)
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
    Console.err.println("Infiles: " + inFiles.reduceLeft(_ + ", " + _))
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
