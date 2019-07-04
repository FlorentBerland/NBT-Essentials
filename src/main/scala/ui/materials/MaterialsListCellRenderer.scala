package ui.materials

import java.awt.{Color, Component, FlowLayout}

import javax.swing.{JLabel, JList, JPanel, ListCellRenderer}
import model.MaterialGroup

class MaterialsListCellRenderer extends ListCellRenderer[MaterialGroup] {

  override def getListCellRendererComponent(list: JList[_ <: MaterialGroup],
                                            value: MaterialGroup,
                                            index: Int,
                                            isSelected: Boolean,
                                            cellHasFocus: Boolean): Component = {

    new JPanel(){
      setLayout(new FlowLayout(FlowLayout.LEFT))
      add(new JLabel(value.name))
      if(isSelected) setBackground(new Color(160, 180, 230))
      if(hasFocus) setBackground(new Color(160, 180, 230))
      value.materialsToWeights.foreach(kv => {
        add(new JLabel(kv._1.name + ": " + kv._2))
      })
    }
  }

}
