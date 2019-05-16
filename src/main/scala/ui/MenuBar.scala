package ui

import java.io.File

import io.ObjFileReader2
import javax.swing.filechooser.FileFilter
import javax.swing.{JFileChooser, JMenu, JMenuBar, JMenuItem, JSeparator}

import scala.util.{Failure, Success}

class MenuBar(store: DataStore) extends JMenuBar {

  add(new JMenu("File"){
    add(new JMenuItem("Open..."){
      addActionListener(_ => {
        val fileChooser = new JFileChooser(){
          addChoosableFileFilter(new FileFilter {
            override def accept(f: File): Boolean = !f.isFile || f.getName.endsWith(".obj")
            override def getDescription = "Wavefront files (*.obj)"
          })
          setFileFilter(getChoosableFileFilters()(1))
        }
        if(fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION){
          loadObjAndUpdateRecentFiles(fileChooser.getSelectedFile.getAbsolutePath)
        }
      })
    })
    add(new JMenu("Open recent"){
      store.recentFiles.addListener(files => {
        removeAll()
        files.foreach(file => {
          add(new JMenuItem(file.split(File.separatorChar).last){
            addActionListener(_ => {
              loadObjAndUpdateRecentFiles(file)
            })
          })
        })
      })
    })
    add(new JSeparator())
    add(new JMenuItem("Export..."){
      addActionListener(_ => println("Not implemented"))
    })
    add(new JSeparator())
    add(new JMenuItem("Exit"){
      addActionListener(_ => System.exit(0))
    })
  })


  private def loadObjAndUpdateRecentFiles(fileName: String): Unit = {
    ObjFileReader2.read(fileName) match {
      case Success(objects) =>
        store.objectsToSelection() = objects.map(_ -> new Mutable(true)).toMap
        val files = store.recentFiles()
        val (l1, l2) = files.splitAt(files.indexOf(fileName) + 1)
        store.recentFiles() = fileName +: (l1.take(l1.size - 1) ++ l2)
      case Failure(ex) =>
        ex.printStackTrace()
        store.objectsToSelection() = Map.empty
        store.recentFiles() = store.recentFiles().filterNot(_ == fileName)
    }
  }

}
