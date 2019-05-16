package ui

import java.awt.{BorderLayout, Dimension, Toolkit}
import java.io.{File, FileOutputStream}

import javax.swing._
import javax.swing.border.{BevelBorder, TitledBorder}

import scala.io.{BufferedSource, Source}
import scala.util._

class MainFrame extends JFrame {

  val store = new DataStore()

  setLayout(new BorderLayout())
  add(new MenuBar(store), BorderLayout.NORTH)
  add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
    new ObjectSelector(store),
    new JSplitPane(JSplitPane.VERTICAL_SPLIT,
      new SelectionInfoPanel(store),
      new MaterialGroups(store)
    )
  ){ setDividerLocation(175) },
    BorderLayout.CENTER)
  add(new JPanel(){
    setBorder(new BevelBorder(BevelBorder.LOWERED))
    add(new MemoryDisplay(1000, MemoryDisplay.Kilobyte))
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
