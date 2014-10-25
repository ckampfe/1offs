package com.ckampfe.reorder_columns

object KeyHandler {
  def apply(keyPress: String, userState: UserState) = {
    keyPress match {
      case "Up"     => moveUp(userState)
      case "Down"   => moveDown(userState)
      case "Escape" => modeUp(userState)
      case "Enter"  => modeDown(userState)
      case _        => userState
    }
  }

  /********************************************************/

  def moveUp(userState: UserState) = userState match {
    case UserState("selecting", cols, selIndex) =>
      if (selIndex > 0)
        userState.copy(selectionIndex = selIndex - 1)
      else userState
    case UserState("ordering", cols, selIndex)  =>
      if (selIndex > 0)
        userState.copy(
          columns = swapUp(
            cols, selIndex
          ),
          selectionIndex = selIndex - 1
        )
      else
        userState
    case _ => userState
  }

  def moveDown(userState: UserState) = userState match {
    case UserState("selecting", cols, selIndex) =>
      if (selIndex < cols.length - 1)
        userState.copy(selectionIndex = selIndex + 1)
      else
        userState
    case UserState("ordering", cols, selIndex)  =>
      if (selIndex < cols.length - 1)
        userState.copy(
          columns = swapDown(cols, selIndex),
          selectionIndex = selIndex + 1
        )
      else
        userState
    case _ => userState
  }

  /********************************************************/

  def modeUp(userState: UserState) = userState match {
    case UserState("selecting", _, _) =>
      userState.copy(mode = "parsing")
    case UserState("ordering", _, _) =>
      userState.copy(mode = "selecting")
    case _ => userState
  }

  def modeDown(userState: UserState) = userState match {
    case UserState("selecting", _, _) =>
      userState.copy(mode = "ordering")
    case _ => userState
  }

  /********************************************************/

  def swapUp[T](columns: List[T], selIndex: Int) =
    if (selIndex > 0) swap(columns, selIndex)
    else columns

  def swapDown[T](columns: List[T], selIndex: Int) =
    if (selIndex < columns.length - 1) swap(columns, selIndex + 1)
    else columns

  def swap[T](columns: List[T], selIndex: Int) =
    columns.patch(
      selIndex - 1,
      List(columns(selIndex), columns(selIndex - 1)),
      2
    )
}
