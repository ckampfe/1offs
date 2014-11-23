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
  val system = ActorSystem("csvSystem")

  var userState           = UserState("selecting", List[String](), 0)

  var newHeaders          = List[String]()
  var originalHeaders     = List[String]()
  var inName              = ""
  var outName             = ""
  var separatorChar: Char = '\u0001'

  def main(args: Array[String]) = {
    inName  = args(0)
    outName = args(1)
    separatorChar  = args.lift(2) match {
      case None    => '\u0001'
      case Some(c) => c.toCharArray.apply(0)
    }

    originalHeaders = getHeaders(inName)
    // lets us print original headers
    userState = UserState("selecting", originalHeaders, 0)
    setUpKeyListening()
    blank(); blank()
  }

  override def nativeKeyPressed(event: NativeKeyEvent) {
    val keyPress = NativeKeyEvent.getKeyText(event.getKeyCode)

    userState = KeyHandler(keyPress, userState)

    userState match {
      case UserState("parsing", newHeaders, _) =>
        GlobalScreen.getInstance().removeNativeKeyListener(this)
        GlobalScreen.unregisterNativeHook()

        val csvWriter = new CSVWriter(
          new FileWriter(outName),
          separatorChar.charValue(),
          CSVWriter.NO_QUOTE_CHARACTER
        )

        val csvReader = new CSVReader(
          new FileReader(inName),
          ',',
          '"',
          '|'
        )

        // actors
        val writer   = system.actorOf(Writer.props(csvWriter))
        val arranger = system.actorOf(
          Arranger.props(originalHeaders, newHeaders, writer)
        )
        val reader   = system.actorOf(Reader.props(csvReader, arranger))

        // kick off processing
        reader ! ("start", newHeaders)
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

  def getHeaders(inName: String) = {
    new CSVReader(
      new FileReader(inName),
      ',',
      '"',
      '|'
    ).readNext().toList
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
