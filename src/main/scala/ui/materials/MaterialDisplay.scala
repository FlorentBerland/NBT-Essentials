package ui.materials

import java.awt.{Dimension, GridBagConstraints, GridBagLayout}

import javax.swing.{JButton, JLabel}
import javax.swing.border.BevelBorder
import model.Material

class MaterialDisplay(mat: Material, weight: Double) extends JButton {

  private var _material = mat
  private var _weight = weight
  private val _constraints = new GridBagConstraints()

  setBorder(new BevelBorder(BevelBorder.LOWERED))
  setLayout(new GridBagLayout())
  setPreferredSize(new Dimension(30, 30))

  _constraints.fill = GridBagConstraints.CENTER
  add(new JLabel(_material.name){

  }, _constraints)

  _constraints.fill = GridBagConstraints.LAST_LINE_END
  add(new JLabel(_weight.toString){

  }, _constraints)

  def getMaterial: Material = _material
  def getWeight: Double = _weight
  def setValues(material: Material, weight: Double): Unit = {
    _material = material
    _weight = weight
  }

}
