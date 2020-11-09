import scala.collection.mutable.ArrayBuffer
class Table(width: Int, height: Int) {
  val cell_collection: ArrayBuffer[ArrayBuffer[Cell]] = ArrayBuffer.fill[Cell](width, height){
    new EmptyCell
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix > width | iy > height | ix < 0 | iy < 0) {
      None
    }
    else {
      Some(cell_collection(ix)(iy))
    }
  }
  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if ((ix < width && iy < height) && ix >= 0 && iy >= 0) {
      cell_collection(ix)(iy) = cell
    }
  }
}