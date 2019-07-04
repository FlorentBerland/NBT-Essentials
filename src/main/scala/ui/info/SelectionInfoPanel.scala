package ui.info

import java.awt.GridLayout

import javax.swing.border.TitledBorder
import javax.swing.{JLabel, JPanel}
import model.mesh.ObjectInfo
import ui.DataStore

class SelectionInfoPanel(store: DataStore) extends JPanel {

  setBorder(new TitledBorder("Selection info"))
  setLayout(new GridLayout(3, 4))
  private var _oldObjectsToSelection = store.objectsToSelection
  private val _widthDisplay = new JLabel()
  private val _heightDisplay = new JLabel()
  private val _lengthDisplay = new JLabel()
  private val _objectsDisplay = new JLabel()
  private val _facesDisplay = new JLabel()
  private val _verticesDisplay = new JLabel()

  add(new JLabel("Width (X axis):"))
  add(_widthDisplay)
  add(new JLabel("Selected objects:"))
  add(_objectsDisplay)
  add(new JLabel("Height (Y axis):"))
  add(_heightDisplay)
  add(new JLabel("Faces:"))
  add(_facesDisplay)
  add(new JLabel("Length (Z axis):"))
  add(_lengthDisplay)
  add(new JLabel("Vertices:"))
  add(_verticesDisplay)

  store.addListener(store => {
    if(store.objectsToSelection != _oldObjectsToSelection){
      val info = store.objectsToSelection
        .filter(_._2)
        .map(_._1.objectInfo) match {
          case collection if collection.nonEmpty => collection.reduce(_ merge _)
          case _ => ObjectInfo.default
        }
      _widthDisplay.setText(Math.ceil(info.xMax - info.xMin).toInt.toString)
      _heightDisplay.setText(Math.ceil(info.yMax - info.yMin).toInt.toString)
      _lengthDisplay.setText(Math.ceil(info.zMax - info.zMin).toInt.toString)
      _objectsDisplay.setText(store.objectsToSelection.count(_._2).toString)
      _facesDisplay.setText(info.nbFaces.toString)
      _verticesDisplay.setText(info.nbVertices.toString)

      _oldObjectsToSelection = store.objectsToSelection
    }
  })

}
