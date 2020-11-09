trait Cell
class EmptyCell() extends Cell {
  override def toString: String = {
    "empty"
  }
}
class NumberCell(number: Int) extends Cell {
  override def toString: String = {
    number.toString
  }
}
class StringCell(text: String) extends Cell {
  override def toString: String = {
    text
  }
}

/* Четвертый тест не прошел */
class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  val cell: Option[Cell] = table.getCell(ix, iy)
  val ref_table: Table = table
  override def toString: String = {
    cell match {
      case Some(ref_cell: ReferenceCell) if (this == ref_cell.ref_table.getCell(ix, iy).get) => { "cyclic "}
      case Some(simple_cell: Cell) => { simple_cell.toString }
      case None => { "outOfRange" }
    }
  }
}

