package ui

import model.MaterialGroup
import model.mesh.Object

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class DataStore {

  private var listeners = new ListBuffer[DataStore => Unit]()

  private var _objectsToSelection = Map.empty[Object, Boolean]
  private var _materialGroups = List.empty[MaterialGroup]
  private var _defaultMaterialGroups = List.empty[MaterialGroup]
  private var _recentFiles = List.empty[String]
  private var _selectedMaterialGroup = Option.empty[MaterialGroup]
  private var _exportedSchematicFile = Option.empty[String]

  def objectsToSelection = _objectsToSelection
  def materialGroups = _materialGroups
  def recentFiles = _recentFiles
  def defaultMaterialGroups = _defaultMaterialGroups
  def selectedMaterialGroup = _selectedMaterialGroup
  def exportedSchematicFile = _exportedSchematicFile

  def addListener(listener: DataStore => Unit): Unit = listeners += listener
  def removeListener(listener: DataStore => Unit): Unit = listeners -= listener

  def execute(command: Command): Unit = {
    command match {
      case SetRecentFiles(Success(files)) => _recentFiles = files.take(5)
      case SetRecentFiles(Failure(exception)) => exception.printStackTrace()
      case LoadFile(file, Success(data)) =>
        _objectsToSelection = data.map(_ -> true).toMap
        if(_recentFiles.contains(file)){
          val (l1, l2) = _recentFiles.splitAt(_recentFiles.indexOf(file) + 1)
          _recentFiles = file +: (l1.take(l1.size - 1) ++ l2)
        } else _recentFiles = (file +: _recentFiles).take(5)
        _materialGroups = _objectsToSelection.keySet
          .flatMap(obj => obj.materialsToFaces.keySet ++ obj.materialsToLines.keySet).toList
          .map(mat => _defaultMaterialGroups.find(_.name == mat.name).getOrElse(mat))
      case LoadFile(file, Failure(exception)) =>
        exception.printStackTrace()
        _recentFiles = _recentFiles diff file
      case CheckObject(obj, isSelected) => _objectsToSelection = _objectsToSelection.updated(obj, isSelected)
      case CheckAll(checked) => _objectsToSelection = _objectsToSelection.map(_._1 -> checked)
      case SelectMaterialGroup(s @ Some(_)) => _selectedMaterialGroup = s
      case SelectMaterialGroup(None) => _selectedMaterialGroup = None
      case SetDefaultMaterialGroups(Success(groups)) =>
        _defaultMaterialGroups = groups
      case SetDefaultMaterialGroups(Failure(exception)) => //exception.printStackTrace()
      case SetExportedSchematicFile(file) => _exportedSchematicFile = Some(file)
    }
    listeners.foreach(_(this))
  }

}

sealed trait Command
sealed case class SetRecentFiles(files: Try[List[String]]) extends Command
sealed case class LoadFile(file: String, data: Try[List[Object]]) extends Command
sealed case class CheckObject(obj: Object, isSelected: Boolean) extends Command
sealed case class CheckAll(checked: Boolean) extends Command
sealed case class SelectMaterialGroup(group: Option[MaterialGroup]) extends Command
sealed case class SetDefaultMaterialGroups(groups: Try[List[MaterialGroup]]) extends Command
sealed case class SetExportedSchematicFile(file: String) extends Command