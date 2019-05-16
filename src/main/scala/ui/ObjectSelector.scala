package ui

import javax.swing.border.TitledBorder
import javax.swing.{BoxLayout, JCheckBox, JPanel, JScrollPane}

class ObjectSelector(store: DataStore) extends JScrollPane {

  setBorder(new TitledBorder("Objects to export"))

  getViewport.add(new JPanel(){
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
    store.objectsToSelection.addListener(objects => {
      removeAll()
      add(new JCheckBox("All") {
        setSelected(true)
        addActionListener(_ => store.objectsToSelection().foreach(_._2() = isSelected))
      })
      objects.foreach(kv => {
        add(new JCheckBox(kv._1.name) {
          setSelected(kv._2())
          addActionListener(_ => {
            store.objectsToSelection()(kv._1)() = isSelected
          })
          store.objectsToSelection()(kv._1).addListener(setSelected)
        })
      })
    })
  })

}
