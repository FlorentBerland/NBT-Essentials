package ui

import java.io.{File, FileOutputStream}

import converters.ObjectToSchematicConverter
import model.mesh.{Object, ObjectInfo}
import io.{NBTFileWriter, ObjFileReader}
import javax.swing.filechooser.FileFilter
import javax.swing.{JFileChooser, JMenu, JMenuBar, JMenuItem, JSeparator}
import model.{MaterialGroup, Schematic}

import scala.util.{Failure, Success}


class MenuBar(store: DataStore) extends JMenuBar {

  private val objFileChooser = new JFileChooser(){
    addChoosableFileFilter(new FileFilter {
      override def accept(f: File): Boolean = !f.isFile || f.getName.endsWith(".obj")
      override def getDescription = "Wavefront files (*.obj)"
    })
    setFileFilter(getChoosableFileFilters()(1))
  }
  private val schematicFileChooser = new JFileChooser(){
    addChoosableFileFilter(new FileFilter {
      override def accept(f: File): Boolean = !f.isFile || f.getName.endsWith(".schematic")
      override def getDescription = "Schematic files (*.schematic)"
    })
    setFileFilter(getChoosableFileFilters()(1))
  }

  add(new JMenu("File"){
    add(new JMenuItem("Open..."){
      addActionListener(_ => {
        if(schematicFileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION){
          store.execute(LoadFile(
            schematicFileChooser.getSelectedFile.getAbsolutePath,
            ObjFileReader.read(schematicFileChooser.getSelectedFile.getAbsolutePath)
          ))
        }
      })
    })
    add(new JMenu("Open recent"){
      private var _oldRecentFiles = store.recentFiles
      store.addListener(store => {
        if(_oldRecentFiles != store.recentFiles){
          removeAll()
          store.recentFiles.foreach(file => add(new JMenuItem(file){
            addActionListener(_ => store.execute(LoadFile(file, ObjFileReader.read(file))))
          }))
          _oldRecentFiles = store.recentFiles
        }
      })
    })
    add(new JSeparator())
    add(new JMenuItem("Save schematic"){
      addActionListener(_ => println("Not implemented"))
    })
    add(new JMenuItem("Save schematic as..."){
      addActionListener(_ => {
        if(schematicFileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION){

        }
      })
    })
    add(new JSeparator())
    add(new JMenuItem("Quit"){
      addActionListener(_ => System.exit(0))
    })
  })

  private def exportSchematic(file: String, objects: List[Object]): Unit = {
    objects match {
      case nonEmptyList if nonEmptyList.nonEmpty =>
        val info = objects.map(_.objectInfo).reduce(_ merge _)
        if(info.nbBadFaces > 0) println("Ignore " + info.nbBadFaces + " non-triangle faces")
        val emptyGroups = objects.map(o => o.materialsToLines.count(_._1.materialsToWeights.isEmpty) + o.materialsToFaces.count(_._1.materialsToWeights.isEmpty)).sum
        if(emptyGroups > 0) println("Found " + emptyGroups + " empty groups")

        val schematic = ObjectToSchematicConverter.createSchematic(objects, ObjectToSchematicConverter.Default)

        NBTFileWriter.write(file, schematic.toNBT) match {
          case Success(_) => println("Schematic saved")
          case Failure(exception) => exception.printStackTrace()
        }
      case _ =>
        println("Empty list of objects !")
    }
  }

}
