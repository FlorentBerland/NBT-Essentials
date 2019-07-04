package ui

import java.awt.{BorderLayout, Dimension, Toolkit}

import javax.swing._
import javax.swing.border.BevelBorder
import ui.info.{MemoryDisplay, SelectionInfoPanel}
import ui.materials.MaterialGroupsPanel
import ui.objects.ObjectsPanel

class MainFrame extends JFrame {

  val store = new DataStore()

  setTitle("NBT-Essentials")
  setLayout(new BorderLayout())
  add(new MenuBar(store), BorderLayout.NORTH)
  add(new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
    new ObjectsPanel(store),
    new JPanel(){
      setLayout(new BorderLayout())
      add(new SelectionInfoPanel(store), BorderLayout.NORTH)
      add(new MaterialGroupsPanel(store), BorderLayout.CENTER)
      add(new MaterialGroupEditor(store), BorderLayout.EAST)
    }
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

  val configManager = new ConfigManager(store)

}
