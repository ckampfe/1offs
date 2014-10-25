package com.ckampfe.reorder_columns

import org.jnativehook.GlobalScreen
import org.jnativehook.keyboard.NativeKeyEvent
import org.jnativehook.keyboard.NativeKeyListener

object Main extends NativeKeyListener {
  // val sampleTestList = List("id", "name", "kind", "size", "created_at", "updated_at")

  var csv: CsvIO = null
  var originalHeaders = List[String]()
  var userState = UserState("selecting", originalHeaders, 0)

  def main(args: Array[String]) = {
    // SET UP CSV
    val inName         = args(0)
    val outName        = args(1)
    val separatorChar  = args.lift(2) match {
      case None    => '\u0001'
      case Some(c) => c.toCharArray.apply(0)
    }

    csv = CsvIO(inName, outName, separatorChar)
    userState = UserState("selecting", csv.originalHeaders, 0)

    // SET UP KEYPRESSES
    GlobalScreen.registerNativeHook()
    GlobalScreen.getInstance().addNativeKeyListener(this)
    blank()
    val input = readLine()
    blank()
  }

  override def nativeKeyPressed(event: NativeKeyEvent) {
    val keyPress = NativeKeyEvent.getKeyText(event.getKeyCode)

    userState = KeyHandler(keyPress, userState)

    userState match {
      case UserState("parsing", newHeaders, _) =>
        println("parsing!")
        csv.writeToOutputFile(newHeaders)
        GlobalScreen.unregisterNativeHook()
      case _ => printColumns(userState)
    }
  }

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

  def blank() = {
    print("\033[2J")
    print(s"\033[${120}A")
    print("\b" * 40)
  }

  override def nativeKeyReleased(event: NativeKeyEvent) {}
  override def nativeKeyTyped(event: NativeKeyEvent) {}
}
