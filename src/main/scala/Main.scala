import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import Graphs.GraphInterpreter
import better.files._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Main {

  case class Def(
    kind: String,
    name: String,
    file: File,
    lineNr: Int,
    line: String,
    usedDefs: Set[String]
  ) {
    def print: String = {
      s"""
         |BEGIN name: $name
         | defined in ${file.name} line $lineNr
         | $line
         | uses: ${usedDefs.mkString(",")}
         |END $name
         |""".stripMargin
    }
  }

  private implicit def gi: GraphInterpreter[Def, String] = new GraphInterpreter[Def, String] {
    override def neighbours(node: Def): Iterable[String] = node.usedDefs

    override def id(node: Def): String = node.name

    override def withNeighbours(node: Def, newNeighbours: Set[String]): Def = node.copy(usedDefs = newNeighbours)
  }

  /** checks if f1 appears before f2 in theory order */
  def fileBefore(f1: File, f2: File, theoryDeps: Map[String, Set[String]]): Boolean = {
    val t1 = f1.name.replaceAll(".thy", "")
    val t2 = f2.name.replaceAll(".thy", "")
    val visited = mutable.Set[String]()

    def search(where: String, t: String): Boolean = {
      if (visited.contains(where)) {
        return false
      }
      visited.addOne(where)
      t == where || (theoryDeps.get(where) match {
        case Some(value) => value.exists(w => search(w, t))
        case None => false
      })
    }


    t1 != t2 && search(t2, t1)
  }

  def removeSameLaterInSameFile(infos: List[Def], theoryDeps: Map[String, Set[String]]): List[Def] = {
    for (i <- infos) yield {
      val newUsed = i.usedDefs.filterNot(u =>
        infos.exists(i2 =>
          i2.usedDefs.contains(u)
            && (i2.file == i.file && i2.lineNr < i.lineNr
            || fileBefore(i2.file, i.file, theoryDeps)))
      )
      i.copy(usedDefs = newUsed)
    }
  }

  def main(args: Array[String]): Unit = {

    val folderName: String = args match {
      case Array(f) => f
      case _ => "."
    }
    val folder = File(folderName)


    println(s"Parsing folder $folder")

    val isabelleFiles: List[File] = folder.glob("{**/*.thy,*.thy}").toList

    println(s"Files  = ${isabelleFiles.map(_.name)}")

    val remove = Set("simp")
    var infos: List[Def] = isabelleFiles.flatMap(parseFile)
      .filter(i => !remove.contains(i.name))



    val definedNames: Set[String] = infos.iterator.map(_.name).toSet

    val theories = infos.filter(_.kind == "theory")

    val theoryNames = theories.map(_.name).toSet

    // theory to imported theories
    val theoryDeps: Map[String, Set[String]] =
      theories.map(t => t.name -> t.usedDefs.intersect(theoryNames)).toMap

    infos = infos.map(d => d.copy(usedDefs = d.usedDefs.intersect(definedNames) - d.name))

    infos = removeSameLaterInSameFile(infos, theoryDeps)

    infos = Graphs.removeTransitive(infos)


    dotGraph(infos)

    htmlVivaGraph(infos)

//    for ((a, bs) <- theoryDeps) {
//      println(s"$a imports ${bs.mkString(", ")}")
//    }

  }


  private def parseFile(isaFile: File): List[Def] = {
    val content = isaFile.lines


    val definitions = new ListBuffer[Def]()
    val uses = new mutable.HashSet[String]()


    val definitionWords = Set("lemma", "fun", "function", "abbreviation", "primrec", "theorem", "definition", "abbreviation", "theory", "datatype", "record", "schematic_goal")
    var currentDef: Option[Def] = None

    def finishCurrentDef(): Unit = {
      currentDef match {
        case Some(d) =>
          definitions.addOne(d.copy(usedDefs = uses.toSet))
        case None =>
      }
      uses.clear()
      currentDef = None
    }

    for ((line, lineNr) <- content.zipWithIndex) {
      val words = line.split("[^a-zA-Z\\-_0-9']+")
      var skip = 0

      if (!line.matches("^\\s*lemma\\s+\".*")) {


        if (words.length >= 2 && definitionWords.contains(words(0))) {
          finishCurrentDef()
          currentDef = Some(Def(words(0), words(1), isaFile, lineNr, line, Set()))
          skip = 2
        } else if (words.nonEmpty && words(0) == "begin" && currentDef.map(_.kind).contains("theory")) {
          finishCurrentDef()
        }

        for (word <- words.iterator.drop(skip)) {
          if (word.nonEmpty)
            uses.add(word)
        }
      }
    }
    finishCurrentDef()

    definitions.toList
  }

  private def htmlVivaGraph(infos2: List[Def]): Unit = {
    val text = getResource("web/template.html")

    val graphJs = new StringBuilder

    for (i <- infos2) {
      graphJs.append(
        s"""
           |graph.addNode("${i.name}", {filename: "${i.file.name}", lineNr: ${i.lineNr}});
           |""".stripMargin)

      for (u <- i.usedDefs) {
        graphJs.append(
          s"""
             |graph.addLink("$u", "${i.name}");
             |""".stripMargin)
      }
    }


    val text2 = text.replace("__graph_js__", graphJs)
    File("graph.html").write(text2)
    val vivagraphJs = getResource("web/vivagraph.min.js")
    File("vivagraph.min.js").write(vivagraphJs)

  }

  private def getResource(resource: String) = {
    new java.util.Scanner(getClass.getResourceAsStream(resource), "UTF-8").useDelimiter("\\A").next
  }

  private def dotGraph(infos2: List[Main.Def]): Unit = {
    val sb = new StringBuilder
    sb.append("digraph G {\n")
    for (i <- infos2) {
      sb.append(s"""  "${i.name}"[label="${i.name}\n${i.file.name}:${i.lineNr}"];""").append("\n")

      for (d <- i.usedDefs) {
        sb.append(s"""  "$d" -> "${i.name}";""").append("\n")
      }
    }
    sb.append("}\n")

    val dot = sb.toString
    File("graph.dot").write(dot)


    def dotIs = new ByteArrayInputStream(dot.getBytes(StandardCharsets.UTF_8))

    import sys.process._
    val reducedDot: String = ("tred" #< dotIs).!!
    File("graph-red.dot").write(reducedDot)

    def dotIs2 = new ByteArrayInputStream(reducedDot.getBytes(StandardCharsets.UTF_8))


    val outputSvg = (s"dot -Tsvg " #< dotIs2).!!

    File("graph.svg").write(outputSvg)
  }
}