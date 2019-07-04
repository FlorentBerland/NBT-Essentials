package ui.materials.chooser

import java.awt.{BorderLayout, GridLayout}

import javax.swing.{JButton, JPanel}
import model.{Material, MaterialGroup}

class MaterialsChooser(group: MaterialGroup) extends JPanel {

  setLayout(new BorderLayout())
  add(new JPanel(){
    setLayout(new GridLayout(0, 9))
    /*store.selectedMaterialGroup.addListener {
      case Some(materialGroup) =>
        materialGroup._2.addListener(materialsToWeights => {
          removeAll()
          println(materialsToWeights.size)
          materialsToWeights.foreach(materialToWeight => {
            add(new JButton(materialToWeight._1.name){
              setPreferredSize(new Dimension(50, 50))
              setSize(new Dimension(50, 50))
              addActionListener(_ => materialGroup._2() = materialGroup._2() - materialToWeight._1)
            })
          })
        })
      case None =>
        removeAll()
    }*/
  }, BorderLayout.NORTH)
  add(new JPanel(){
    setLayout(new GridLayout(0, 9))
    Material.all.foreach(nameToMaterial => {
      add(new JButton(nameToMaterial._1){
        // TODO: Add the material
      })
    })
  }, BorderLayout.CENTER)

}
