package ui.objects

import javax.swing.border.TitledBorder
import javax.swing.{BoxLayout, JCheckBox, JPanel, JScrollPane}
import ui._

class ObjectsPanel(store: DataStore) extends JScrollPane {

  setBorder(new TitledBorder("Objects to export"))

  getViewport.add(new JPanel(){
    private var _oldObjectsToSelection = store.objectsToSelection
    private var _checkBoxes = store.objectsToSelection
      .keys.map(objects => new ObjectCheckbox(objects, store))
      .toArray.sortBy(_.getObject.name)
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    store.addListener(store => {
      if(_oldObjectsToSelection != store.objectsToSelection){
        val sortedObjects = store.objectsToSelection.keySet.toArray.sortBy(_.name)
        if(_checkBoxes.length < sortedObjects.length){
          // Add new checkboxes
          _checkBoxes = _checkBoxes ++ sortedObjects.view.slice(_checkBoxes.length, sortedObjects.length).map(obj => {
              val chk = new ObjectCheckbox(obj, store)
              add(chk)
              chk
            })
        } else if(_checkBoxes.length > sortedObjects.length){
          // Remove the excess
          _checkBoxes.view.slice(sortedObjects.length, _checkBoxes.length).foreach(remove)
          _checkBoxes = _checkBoxes.take(sortedObjects.length)
        }
        // Replace the values
        sortedObjects.zipWithIndex.foreach(kv => _checkBoxes(kv._2).setValue(kv._1, store.objectsToSelection(kv._1)))

        _oldObjectsToSelection = store.objectsToSelection
      }
    })

    add(new JCheckBox("Select all") {
      setSelected(true)
      setVisible(_checkBoxes.nonEmpty)
      addActionListener(_ => store.execute(CheckAll(isSelected)))
      store.addListener(store => setVisible(store.objectsToSelection.nonEmpty))
    })
    _checkBoxes.foreach(add)
  })

}
