import converters.ObjectToSchematicConverter
import io._
import io.config.MaterialsIO
import ui.MainFrame
import util.Time


object Main extends App {

  parseWithRemap()

  def parseWithUi(): Unit = {
    val ui = new MainFrame
    ui.setVisible(true)
  }

  def parseWithRemap(): Unit = {
    Time {
      val (inputFile, outputFile) = getInOutFiles
      val objects = Time {
        print("Loading data from " + inputFile + "... ")
        ObjFileReader.read(inputFile).get
      }(t => println("done in " + t + " ms"))
      val materialsRemap = Time {
        print("Loading materials... ")
        MaterialsIO.loadMaterialsGroups().get
      }(t => println("       done in " + t + " ms"))
      val remappedObjects = Time {
        print("Mapping materials... ")
        objects.map(o => model.mesh.Object(
          o.name,
          o.materialsToFaces.map(kv => materialsRemap.find(_.name == kv._1.name).getOrElse(kv._1) -> kv._2),
          o.materialsToLines.map(kv => materialsRemap.find(_.name == kv._1.name).getOrElse(kv._1) -> kv._2)
        ))
      }(t => println("       done in " + t + " ms"))
      val schematic = ObjectToSchematicConverter.createSchematic(remappedObjects, ObjectToSchematicConverter.Default)
      Time {
        print("Saving schematic into " + outputFile + "... ")
        NBTFileWriter.write(outputFile, schematic.toNBT)
      }(t => println("done in " + t + " ms"))
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