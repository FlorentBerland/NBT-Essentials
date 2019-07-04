package ui

import java.awt.{Dimension, GridLayout}

import javax.swing.{JButton, JDialog, JFrame, JPanel}
import javax.swing.border.TitledBorder
import ui.materials.chooser.MaterialsChooserDialog

class MaterialGroupEditor(store: DataStore) extends JPanel {

  setPreferredSize(new Dimension(200, 100))
  setLayout(new GridLayout(0, 6))
  setBorder(new TitledBorder("Edit group") {
    store.addListener(store => {
      store.selectedMaterialGroup match {
        case Some(group) => setTitle(group.name)
        case None => setTitle("Edit group")
      }
      updateUI()
    })
  })
  store.addListener(_.selectedMaterialGroup match {
    case Some(materialGroup) =>
      removeAll()
      add(new JButton("+"){
        addActionListener(_ => new MaterialsChooserDialog(materialGroup, this).showDialog())
      })
      materialGroup.materialsToWeights.foreach(materials => {
        add(new JButton(materials._1.name + ": " + materials._2){
          addActionListener(_ => new MaterialsChooserDialog(materialGroup, this).showDialog())
        })
      })
    case None => removeAll()
  })

}
