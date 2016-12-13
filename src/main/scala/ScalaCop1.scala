object ScalaCop1 extends App {
  def double(a: Int): Long = 2 * a.toLong

  def mergeLinesRecursively(line: List[Cell]): List[Cell] = line match {
    case a::b::tail => {
      val (first, second) = a.merge(b)

      first::mergeLinesRecursively(second::tail)
    }
    case _ => line
  }

  def mergeLine(line: List[Cell]): List[Cell] = {
    val (zeros, nonzeros) = line.partition(_.tile == 0)

    zeros ++ mergeLinesRecursively(nonzeros)
  }
}

case class Cell(tile: Int) {
  def merge(anotherCell: Cell): (Cell, Cell) = anotherCell match {
    case Cell(a) if a == tile => (Cell(0), Cell(tile * 2))
    case _ => (this, anotherCell)
  }
}
