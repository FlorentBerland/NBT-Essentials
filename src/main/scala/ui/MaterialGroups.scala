package ui

import javax.swing.border.TitledBorder
import javax.swing._

class MaterialGroups(store: DataStore) extends JScrollPane {

  setBorder(new TitledBorder("Materials"))
  getViewport.add(new JList[String](new DefaultListModel[String](){
    store.materialsRemap.addListener(materialGroups => {
      removeAllElements()
      materialGroups.foreach(kv => addElement(kv._1.name))
    })
  }))

}
