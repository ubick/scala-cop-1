import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}
import ScalaCop1._

object ScalaCop1Specification extends Properties("ScalaCop1") {
  property("the amount of cells is the same") = forAll {
    (list: List[Int]) => {
      val line = list.map(Cell(_))

      mergeLine(line).length == line.length
    }
  }

  property("the sum of the line should be the same after merging") = forAll {
    (list: List[Int]) => {
      val line = list.map(Cell(_))

      mergeLine(line).map(_.tile).sum == line.map(_.tile).sum
    }
  }

  property("combining 2 numbers should result in 0 and the sum") = forAll {
    (n: Int) => {
      val line = List(Cell(n), Cell(n))

      mergeLine(line) == List(Cell(0), Cell(2 * n))
    }
  }

  property("moves spaces to the left") = forAll(Gen.choose(0, 100), Arbitrary.arbitrary[List[Int]]) {
    (n: Int, list: List[Int]) => {
      val line = (list++List.fill(n)(0)).map(Cell(_))

      mergeLine(line).startsWith(List.fill(n)(0).map(Cell(_)))
    }
  }

  property("even amount of same cells results in half as many non-zero cells after merging") =
    forAll(Gen.choose(0, 100)) {
      (n: Int) => {
        val line = List.fill(n*2)(n).map(Cell(_))

        mergeLine(line).count(_.tile == n * 2) == n
      }
    }
}