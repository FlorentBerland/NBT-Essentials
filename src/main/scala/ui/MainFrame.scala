package ui

import java.awt.{BorderLayout, Toolkit}
import java.io.File

import io.ObjFileReader2
import model.mesh.Object
import javax.swing._
import javax.swing.border.BevelBorder
import javax.swing.filechooser.FileFilter

import scala.util.Try

class MainFrame extends JFrame {

  var data: Try[List[Object]] = _

  setLayout(new BorderLayout())
  add(new JMenuBar(){
    add(new JMenu("File"){
      add(new JMenuItem("Open..."){
        addActionListener(_ => {
          val fileChooser = new JFileChooser(){
            addChoosableFileFilter(new FileFilter {
              override def accept(f: File) = !f.isFile || f.getName.endsWith(".obj")
              override def getDescription = "Wavefront files (*.obj)"
            })
          }
          if(fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION){
            data = ObjFileReader2.read(fileChooser.getSelectedFile.getAbsolutePath)
            println(data)
          }
        })
      })
      add(new JMenu("Open recent"){
        addActionListener(_ => notImplemented())
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
        addActionListener(_ => notImplemented())
      })
    })
  }, BorderLayout.NORTH)
  add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
    new JPanel(),
    new JPanel()
  ), BorderLayout.CENTER)
  add(new JPanel(){
    setBorder(new BevelBorder(BevelBorder.LOWERED))
    val memoryLabel = new JLabel()
    add(memoryLabel)
    val timer = new Timer(
      1000,
      _ => memoryLabel.setText("Memory usage: " +
        (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / 1024 / 1024
        + " Mo")
    ){ start() }
  }, BorderLayout.SOUTH)
  pack()
  val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  setLocation((screenSize.width - getSize.width) / 2, (screenSize.height - getSize.height) / 2)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  def notImplemented(): Unit = println("Not implemented yet")

}
