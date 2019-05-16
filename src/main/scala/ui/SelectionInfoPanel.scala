package ui

import java.awt.GridLayout

import javax.swing.border.TitledBorder
import javax.swing.{JLabel, JPanel}
import model.mesh.ObjectInfo

class SelectionInfoPanel(store: DataStore) extends JPanel {

  setBorder(new TitledBorder("Selection info"))
  setLayout(new GridLayout(3, 4))
  val getInfo = () => store.objectsToSelection()
    .filter(_._2())
    .map(_._1.objectInfo) match {
    case collection if collection.nonEmpty => collection.reduce(_ merge _)
    case _ => ObjectInfo.default
  }
  add(new JLabel("Width (X axis):"))
  add(new JLabel(){
    val callback = (_: Boolean) => {
      val info = getInfo()
      val width = Math.ceil(info.xMax - info.xMin)
      setText(width.toInt.toString)
    }
    store.objectsToSelection.addListener(_ => callback(true))
    store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
  })
  add(new JLabel("Selected objects:"))
  add(new JLabel(){
    val callback = (_: Boolean) => {
      setText(store.objectsToSelection().count(_._2()).toString)
    }
    store.objectsToSelection.addListener(_ => callback(true))
    store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
  })
  add(new JLabel("Height (Y axis):"))
  add(new JLabel(){
    val callback = (_: Boolean) => {
      val info = getInfo()
      val height = Math.ceil(info.yMax - info.yMin)
      setText(height.toInt.toString)
    }
    store.objectsToSelection.addListener(_ => callback(true))
    store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
  })
  add(new JLabel("Faces:"))
  add(new JLabel(){
    val callback = (_: Boolean) => {
      setText(getInfo().nbFaces.toString)
    }
    store.objectsToSelection.addListener(_ => callback(true))
    store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
  })
  add(new JLabel("Length (Z axis):"))
  add(new JLabel(){
    val callback = (_: Boolean) => {
      val info = getInfo()
      val length = Math.ceil(info.zMax - info.zMin)
      setText(length.toInt.toString)
    }
    store.objectsToSelection.addListener(_ => callback(true))
    store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
  })
  add(new JLabel("Vertices:"))
  add(new JLabel(){
    val callback = (_: Boolean) => {
      setText(getInfo().nbVertices.toString)
    }
    store.objectsToSelection.addListener(_ => callback(true))
    store.objectsToSelection.addListener(_.foreach(_._2.addListener(callback)))
  })

}
