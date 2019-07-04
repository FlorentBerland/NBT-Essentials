package ui

import java.io.{File, FileOutputStream}

import io.config.{MaterialsIO, RecentFilesIO}
import model.{Material, MaterialGroup}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

class ConfigManager(store: DataStore){

  private var _oldRecentFiles = store.recentFiles
  private var _oldDefaultMaterialGroups = store.defaultMaterialGroups

  store.execute(SetRecentFiles(RecentFilesIO.loadRecentFiles()))
  store.execute(SetDefaultMaterialGroups(MaterialsIO.loadMaterialsGroups()))

  store.addListener(store => {
    if(_oldRecentFiles != store.recentFiles){
      RecentFilesIO.saveRecentFiles(store.recentFiles)
      _oldRecentFiles = store.recentFiles
    }
    if(_oldDefaultMaterialGroups != store.defaultMaterialGroups){
      MaterialsIO.saveMaterialGroups(store.defaultMaterialGroups)
      _oldDefaultMaterialGroups = store.defaultMaterialGroups
    }
  })

}
