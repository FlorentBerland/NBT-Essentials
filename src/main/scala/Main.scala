import converters.ObjectToSchematicConverter
import io._
import io.config.MaterialsIO
import model.mesh.ObjectInfo
import model.{Material, Schematic}
import ui.MainFrame


object Main extends App {

  parseWithRemap()

  def parseWithUi(): Unit = {
    val ui = new MainFrame
    ui.setVisible(true)
  }

  def parseWithRemap(): Unit = {
    Time {
      println("Converting data structure...")
      val (inputFile, outputFile) = getInOutFiles
      val objects = ObjFileReader2.read(inputFile).get
      val materialsRemap = MaterialsIO.loadMaterialsGroups().get
      val remappedObjects = objects.map(o => model.mesh.Object(
        o.name,
        o.materialsToFaces.map(kv => materialsRemap.find(_.name == kv._1.name).getOrElse(kv._1) -> kv._2),
        o.materialsToLines.map(kv => materialsRemap.find(_.name == kv._1.name).getOrElse(kv._1) -> kv._2)
      ))
      val schematic = ObjectToSchematicConverter.createSchematic(remappedObjects, ObjectToSchematicConverter.Default)
      NBTFileWriter.write(outputFile, schematic.toNBT)
    }(t => println("Whole process donne in " + t + " ms"))
  }

  def parseExperimental(): Unit = {
    Time {
      val (inputFile, outputFile) = getInOutFiles

      print("Loading data from " + inputFile + "...")
      val vertices = Time {
        ObjFileReader.read(inputFile).get
      }(t => println(" done in " + t + " ms."))
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
      print("Creating schematic...")
      Time {
        vertices.foreach(v => {
          schem.setBlock(v._1 - xMin, v._2 - yMin, v._3 - zMin, v._4._1.id.toByte)
          schem.setData(v._1 - xMin, v._2 - yMin, v._3 - zMin, v._4._2)
        })
      }(t => println(" done in " + t + " ms"))

      println()
      println("Schematic info:")
      println("\tWidth:  " + width)
      println("\tHeight: " + height)
      println("\tLength: " + length)
      println("\tTotal processed blocks:    " + vertices.length)
      println("\tTotal blocks in schematic: " + width * height * length)
      println()

      print("Creating complete, saving to " + outputFile + "...")
      Time {
        NBTFileWriter.write(outputFile, schem.toNBT)
      }(t => println(" done in " + t + " ms"))
    }(t => println("Whole process done in " + t + " ms"))
  }

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