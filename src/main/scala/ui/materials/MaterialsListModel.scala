package ui.materials

import javax.swing.DefaultListModel
import model.MaterialGroup
import ui.DataStore

class MaterialsListModel(store: DataStore) extends DefaultListModel[MaterialGroup] {

  private var _oldMaterialGroups = store.materialGroups
  store.addListener(store => {
    if(_oldMaterialGroups != store.materialGroups){
      _oldMaterialGroups = store.materialGroups
      removeAllElements()
      store.materialGroups.foreach(addElement)
    }
  })

}
