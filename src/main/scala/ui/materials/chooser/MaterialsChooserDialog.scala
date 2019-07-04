package ui.materials.chooser

import java.awt.{BorderLayout, Component, Dimension, FlowLayout, GridLayout, Toolkit}

import javax.swing.border.BevelBorder
import javax.swing.{JButton, JDialog, JFrame, JLabel, JPanel, JScrollPane, SwingUtilities}
import model.{Material, MaterialGroup}
import ui.materials.MaterialDisplay

class MaterialsChooserDialog(group: MaterialGroup, owner: Component)
  extends JDialog(SwingUtilities.getRoot(owner).asInstanceOf[JFrame], true) {

  private var _editedMaterialGroup = group

  add(new JPanel(){
    setLayout(new BorderLayout())
    add(new JPanel(){
      setLayout(new GridLayout(0, 9))

    }, BorderLayout.NORTH)
    add(new JScrollPane(){
      getViewport.add(new JPanel(){
        setLayout(new FlowLayout(FlowLayout.LEFT))
        Material.all.foreach(nameToMaterial => {
          add(new MaterialDisplay(nameToMaterial._2, _editedMaterialGroup.materialsToWeights.getOrElse(nameToMaterial._2, 0)){
            addActionListener(_ => _editedMaterialGroup = _editedMaterialGroup.updated(nameToMaterial._2, 1))
          })
        })
      })
    })
  })
  pack()
  val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  setLocation((screenSize.width - getSize.width) / 2, (screenSize.height - getSize.height) / 2)

  def showDialog(): Unit = {
    setVisible(true)
  }

  def getMaterialGroup: MaterialGroup = _editedMaterialGroup

}
