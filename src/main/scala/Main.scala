import java.util.Date

import io._
import model.Schematic


object Main extends App {

  val startTime = new Date().getTime
  val (inputFile, outputFile) = getInOutFiles

  println("Loading data from " + inputFile + "...")
  val vertices = ObjFileReader.read(inputFile).get
  println("Loading complete, creating schematic...")
  val xMin = vertices.minBy(_._1)._1
  val xMax = vertices.maxBy(_._1)._1
  val yMin = vertices.minBy(_._2)._2
  val yMax = vertices.maxBy(_._2)._2
  val zMin = vertices.minBy(_._3)._3
  val zMax = vertices.maxBy(_._3)._3
  val width = xMax - xMin + 1
  val height = yMax - yMin + 1
  val length = zMax - zMin + 1
  val schem = Schematic(width, height, length)
  vertices.foreach(v => {
    schem.setBlock(v._1 - xMin, v._2 - yMin, v._3 - zMin, v._4._1)
    schem.setData(v._1 - xMin, v._2 - yMin, v._3 - zMin, v._4._2)
  })

  println()
  println("Schematic info:")
  println("\tWidth:  " + width)
  println("\tHeight: " + height)
  println("\tLength: " + length)
  println("\tTotal processed blocks:    " + vertices.length)
  println("\tTotal blocks in schematic: " + width * height * length)
  println()

  println("Creating complete, saving to " + outputFile + "...")
  NBTFileWriter.write(outputFile, schem.toNBT)
  println("Done in " + (new Date().getTime - startTime) + " ms")


  def getInOutFiles: (String, String) = {
    if(args.isEmpty) throw new NoSuchFieldException("Missing a .obj file name")
    val input = args(0)
    val output = if(args.length >= 2) args(1)
    else {
      val dotIndex = input.lastIndexOf(".")
      if(dotIndex == -1) input + ".schematic"
      else input.splitAt(dotIndex)._1 + ".schematic"
    }
    (input, output)
  }
}