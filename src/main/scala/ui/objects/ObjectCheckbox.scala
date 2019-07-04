package ui.objects

import javax.swing.JCheckBox
import model.mesh.Object
import ui.{CheckObject, DataStore}

class ObjectCheckbox(_obj: Object, store: DataStore) extends JCheckBox {

  private var obj = _obj

  addActionListener(_ => store.execute(CheckObject(obj, isSelected)))

  def setValue(newObject: Object, checked: Boolean): Unit = {
    obj = newObject
    setSelected(checked)
    setText(obj.name)
  }

  def getObject: Object = obj

}
