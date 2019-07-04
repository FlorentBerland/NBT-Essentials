package ui.materials

import javax.swing._
import javax.swing.border.TitledBorder
import model.MaterialGroup
import ui.{DataStore, SelectMaterialGroup}

class MaterialGroupsPanel(store: DataStore) extends JScrollPane {

  setBorder(new TitledBorder("Material groups"))
  getViewport.add(new JList[MaterialGroup](new MaterialsListModel(store)) {
    setCellRenderer(new MaterialsListCellRenderer)
    setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    addListSelectionListener(_ =>
      if(isSelectionEmpty) store.execute(SelectMaterialGroup(None))
      else store.execute(SelectMaterialGroup(Some(getSelectedValue)))
    )
    store.addListener(_.selectedMaterialGroup match {
      case Some(group) => setSelectedIndex(store.materialGroups.indexOf(group))
      case None => setSelectedIndices(Array.empty)
    })
  })

}
