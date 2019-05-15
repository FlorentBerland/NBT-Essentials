package ui

import java.awt.{BorderLayout, Dimension, GridLayout, Toolkit}
import java.io.{File, FileOutputStream}

import io.ObjFileReader2
import javax.swing._
import javax.swing.border.{BevelBorder, TitledBorder}
import javax.swing.filechooser.FileFilter
import model.mesh.ObjectInfo

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
            setFileFilter(getChoosableFileFilters()(1))
          }
          if(fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION){
            val fileName = fileChooser.getSelectedFile.getAbsolutePath
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
                    store.objectsToSelection() = objects.map(_ -> new Mutable(true)).toMap
                    val (l1, l2) = files.splitAt(files.indexOf(file) + 1)
                    store.recentFiles() = file +: (l1.take(l1.size - 1) ++ l2)
                  case Failure(ex) =>
                    ex.printStackTrace()
                    store.objectsToSelection() = Map.empty
                    store.recentFiles() = files.filterNot(_ == file)
                }
              })
            })
          })
        })
      })
      add(new JSeparator())
      add(new JMenuItem("Export..."){
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
      store.objectsToSelection.addListener(objects => {
        removeAll()
        objects.foreach(kv => {
          add(new JCheckBox(kv._1.name){
            setSelected(kv._2())
            addActionListener(_ => {
              store.objectsToSelection()(kv._1)() = isSelected
            })
            store.objectsToSelection()(kv._1).addListener(setSelected)
          })
        })
      })
    }){
      setBorder(new TitledBorder("Objects to export"))
    },
    new JSplitPane(JSplitPane.VERTICAL_SPLIT,
      new JPanel(){
        setBorder(new TitledBorder("Selection info"))
        setLayout(new GridLayout(3, 4))
        val getInfo = () => store.objectsToSelection()
          .filter(_._2())
          .map(_._1.objectInfo) match {
          case collection if collection.nonEmpty => collection.reduce(_ merge _)
          case _ => ObjectInfo.default
        }
        add(new JLabel("Width (X axis):"))
        add(new JLabel(){
          val callback = (_: Boolean) => {
            val info = getInfo()
            val width = Math.ceil(info.xMax - info.xMin)
            setText(width.toInt.toString)
          }
          store.objectsToSelection.addListener(_ => callback(true))
          store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
        })
        add(new JLabel("Selected objects:"))
        add(new JLabel(){
          val callback = (_: Boolean) => {
            setText(store.objectsToSelection().count(_._2()).toString)
          }
          store.objectsToSelection.addListener(_ => callback(true))
          store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
        })
        add(new JLabel("Height (Y axis):"))
        add(new JLabel(){
          val callback = (_: Boolean) => {
            val info = getInfo()
            val height = Math.ceil(info.yMax - info.yMin)
            setText(height.toInt.toString)
          }
          store.objectsToSelection.addListener(_ => callback(true))
          store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
        })
        add(new JLabel("Faces:"))
        add(new JLabel(){
          val callback = (_: Boolean) => {
            setText(getInfo().nbFaces.toString)
          }
          store.objectsToSelection.addListener(_ => callback(true))
          store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
        })
        add(new JLabel("Length (Z axis):"))
        add(new JLabel(){
          val callback = (_: Boolean) => {
            val info = getInfo()
            val length = Math.ceil(info.zMax - info.zMin)
            setText(length.toInt.toString)
          }
          store.objectsToSelection.addListener(_ => callback(true))
          store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
        })
        add(new JLabel("Vertices:"))
        add(new JLabel(){
          val callback = (_: Boolean) => {
            setText(getInfo().nbVertices.toString)
          }
          store.objectsToSelection.addListener(_ => callback(true))
          store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
        })
      },
      new JPanel()
    )
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
        (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / 1000
        + " ko")
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
