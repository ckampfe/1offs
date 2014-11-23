package com.ckampfe.reorder_columns

import akka.actor.ActorSystem
import akka.pattern.ask
import java.io.FileReader
import java.io.FileWriter

import com.opencsv._
import org.jnativehook.GlobalScreen
import org.jnativehook.keyboard.NativeKeyEvent
import org.jnativehook.keyboard.NativeKeyListener

object Main extends NativeKeyListener {

  // top level state initialization
  var csv: CsvIO = null
  var originalHeaders = List[String]()
  var userState = UserState("selecting", originalHeaders, 0)

  def main(args: Array[String]) = {
    csv = setUpCsvReadWrite(args)
    userState = UserState("selecting", csv.originalHeaders, 0)
    setUpKeyListening()
    blank(); blank()
  }

  override def nativeKeyPressed(event: NativeKeyEvent) {
    val keyPress = NativeKeyEvent.getKeyText(event.getKeyCode)

    userState = KeyHandler(keyPress, userState)

    userState match {
      case UserState("parsing", newHeaders, _) =>
        csv.writeToOutputFile(newHeaders)
        GlobalScreen.getInstance().removeNativeKeyListener(this)
        GlobalScreen.unregisterNativeHook()
      case _ => printColumns(userState)
    }
  }

  /********************************************************/

  // setup and utility methods

  def printColumns(userState: UserState) = {
    blank()

    val newState = userState match {
      case UserState("selecting", cols, selIndex) =>
        userState.copy(columns =
          cols.updated(selIndex, "=> " +
            cols(selIndex)))
      case _ =>
        userState.copy(
          columns = userState.columns.updated(
            userState.selectionIndex,
            "[ " + userState.columns(userState.selectionIndex) + " ]"
          )
        )
    }

    newState.columns.map(println)
    print("\b" * 40)
  }

  def setUpCsvReadWrite(args: Array[String]) = {
    val inName         = args(0)
    val outName        = args(1)
    val separatorChar  = args.lift(2) match {
      case None    => '\u0001'
      case Some(c) => c.toCharArray.apply(0)
    }

    CsvIO(inName, outName, separatorChar)
  }

  def setUpKeyListening() = {
    GlobalScreen.registerNativeHook()
    GlobalScreen.getInstance().addNativeKeyListener(this)
  }

  def blank() = {
    print("\033[2J")
    print(s"\033[${120}A")
    print("\b" * 40)
  }

  // these are required by the interface, but not needed
  // in this implementation
  override def nativeKeyReleased(event: NativeKeyEvent) {}
  override def nativeKeyTyped(event: NativeKeyEvent) {}
}
