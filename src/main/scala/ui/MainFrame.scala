package ui

import java.awt.{BorderLayout, Dimension, Toolkit}
import java.io.{File, FileOutputStream}

import io.ObjFileReader2
import model.mesh.Object
import javax.swing._
import javax.swing.border.{BevelBorder, TitledBorder}
import javax.swing.filechooser.FileFilter

import scala.io.{BufferedSource, Source}
import scala.util._

class MainFrame extends JFrame {

  val store = new DataStore()

  setLayout(new BorderLayout())
  add(new JMenuBar(){
    add(new JMenu("File"){
      add(new JMenuItem("Open..."){
        addActionListener(_ => {
          val fileChooser = new JFileChooser(){
            addChoosableFileFilter(new FileFilter {
              override def accept(f: File): Boolean = !f.isFile || f.getName.endsWith(".obj")
              override def getDescription = "Wavefront files (*.obj)"
            })
          }
          if(fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION){
            val fileName = fileChooser.getSelectedFile.getAbsolutePath
            ObjFileReader2.read(fileName) match {
              case Success(objects) =>
                store.objects() = objects
                val files = store.recentFiles()
                val (l1, l2) = files.splitAt(files.indexOf(fileName) + 1)
                store.recentFiles() = fileName +: (l1.init ++ l2)
                store.selectedObjects() = objects.toSet
              case Failure(ex) =>
                ex.printStackTrace()
                store.objects() = List.empty
                store.recentFiles() = store.recentFiles().filterNot(_ == fileName)
            }
          }
        })
      })
      add(new JMenu("Open recent"){
        store.recentFiles.addListener(files => {
          removeAll()
          files.foreach(file => {
            add(new JMenuItem(file.split(File.separatorChar).last){
              addActionListener(_ => {
                ObjFileReader2.read(file) match {
                  case Success(objects) =>
                    store.selectedObjects() = objects.toSet
                    store.objects() = objects
                    val (l1, l2) = files.splitAt(files.indexOf(file) + 1)
                    store.recentFiles() = file +: (l1.init ++ l2)
                  case Failure(ex) =>
                    ex.printStackTrace()
                    store.objects() = List.empty
                    store.recentFiles() = files.filterNot(_ == file)
                }
              })
            })
          })
        })
      })
      add(new JSeparator())
      add(new JMenuItem("Save"){
        addActionListener(_ => notImplemented())
      })
      add(new JMenuItem("Save as..."){
        addActionListener(_ => notImplemented())
      })
      add(new JSeparator())
      add(new JMenuItem("Exit"){
        addActionListener(_ => System.exit(0))
      })
    })
  }, BorderLayout.NORTH)
  add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
    new JScrollPane(new JPanel(){
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
      store.objects.addListener(objects => {
        removeAll()
        objects.foreach(obj => {
          add(new JCheckBox(obj.name){
            setSelected(store.selectedObjects().contains(obj))
            addActionListener(_ => {
              if(isSelected){
                store.selectedObjects() = store.selectedObjects() + obj
              } else {
                store.selectedObjects() = store.selectedObjects() - obj
              }
              println(store.selectedObjects().size)
            })
          })
        })
      })
    }){
      setBorder(new TitledBorder("Objects to export"))
    },
    new JPanel()
  ){
    setDividerLocation(175)
  }, BorderLayout.CENTER)
  add(new JPanel(){
    setBorder(new BevelBorder(BevelBorder.LOWERED))
    val memoryLabel = new JLabel()
    add(memoryLabel)
    val timer = new Timer(
      1000,
      _ => memoryLabel.setText("Memory usage: " +
        (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / 1000000
        + " Mo")
    ){ start() }
  }, BorderLayout.SOUTH)

  setPreferredSize(new Dimension(700, 500))
  pack()
  val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  setLocation((screenSize.width - getSize.width) / 2, (screenSize.height - getSize.height) / 2)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  loadRecentFiles().foreach(store.recentFiles() = _)
  store.recentFiles.addListener(files => {
    if(files.nonEmpty) saveRecentFiles(files)
  })

  def notImplemented(): Unit = println("Not implemented yet")

  def loadRecentFiles(): Try[List[String]] = {
    var source: BufferedSource = null
    try {
      source = Source.fromFile("config/recent")
      Success(source.getLines.toList)
    } catch {
      case e: Exception => Failure(e)
    } finally {
      if(source != null) source.close()
    }
  }

  def saveRecentFiles(files: List[String]): Unit = {
    val configFolder = new File("config")
    if((configFolder.exists() && configFolder.isDirectory) || configFolder.mkdir()) {
      var output: FileOutputStream = null
      try{
        output = new FileOutputStream("config/recent")
        output.write(files.take(5).mkString("\n").getBytes)
      } catch {
        case _: Exception =>
      }
      if(output != null) output.close()
    }
  }

}
