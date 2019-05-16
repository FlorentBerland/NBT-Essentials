package ui

import java.text.NumberFormat
import java.util.Locale

import javax.swing.{JLabel, Timer}
import ui.MemoryDisplay.MemoryUnit

class MemoryDisplay(intervalMillis: Int, unit: MemoryUnit) extends JLabel {

  new Timer(
    intervalMillis,
    _ => {
      val memory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
      setText("Memory usage: " + NumberFormat.getInstance(Locale.US).format(memory / unit.range) + " " + unit.display)
    }
  ){ start() }

}

object MemoryDisplay {

  sealed abstract class MemoryUnit(val display: String, val range: Long)
  case object Byte extends MemoryUnit("B", 1)
  case object Kilobyte extends MemoryUnit("kB", 1000)
  case object KibiByte extends MemoryUnit("KiB", 1024)
  case object MegaByte extends MemoryUnit("MB", 1000 * 1000)
  case object MibiByte extends MemoryUnit("MiB", 1024 * 1024)
  case object GigaByte extends MemoryUnit("GB", 1000 * 1000 * 1000)
  case object GibiByte extends MemoryUnit("GiB", 1024 * 1024 * 1024)

}
