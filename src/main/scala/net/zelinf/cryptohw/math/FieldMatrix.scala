package net.zelinf.cryptohw.math

import cats.implicits._
import scala.reflect.ClassTag

final class FieldMatrix[E: Field: ClassTag] private (
  data: IndexedSeq[IndexedSeq[E]]
) {
  val rowCount: Int = data.size
  val columnCount: Int = if (data.isEmpty) 0 else data.head.size

  private val mat: Vector[Vector[E]] = data.map(_.toVector).toVector

  def *(that: FieldMatrix[E]): Option[FieldMatrix[E]] = {
    if (columnCount != that.rowCount)
      Option.empty
    else {
      val result = Array.fill(rowCount, columnCount)(Field[E].empty)
      for (i <- 0 until rowCount) {
        for (j <- 0 until columnCount) {
          result(i)(j) = FieldMatrix.multiplyVector(row(i), that.column(j))
        }
      }
      Option(new FieldMatrix[E](result.map(_.toVector)))
    }
  }

  def +(that: FieldMatrix[E]): Option[FieldMatrix[E]] = {
    if (rowCount != that.rowCount || columnCount != that.columnCount)
      Option.empty
    else {
      val sum = Array.fill(rowCount, columnCount)(Field[E].empty)
      for (i <- 0 until rowCount) {
        for (j <- 0 until columnCount) {
          sum(i)(j) = Field[E].combine(mat(i)(j), that.mat(i)(j))
        }
      }
      Option(new FieldMatrix[E](sum.map(_.toVector)))
    }
  }

  def apply(row: Int, column: Int): E = {
    mat(row)(column)
  }

  def row(index: Int): Vector[E] = {
    require(0 <= index && index < rowCount)
    mat(index)
  }

  def rows: Stream[Vector[E]] =
    (0 until rowCount).toStream.map(i => row(i))

  def column(index: Int): Vector[E] = {
    require(0 <= index && index < columnCount)
    mat.map(r => r(index))
  }

  def columns: Stream[Vector[E]] =
    (0 until columnCount).toStream.map(i => column(i))
}

object FieldMatrix {

  def apply[E: Field: ClassTag](
    data: IndexedSeq[IndexedSeq[E]]
  ): FieldMatrix[E] = {
    new FieldMatrix[E](data)
  }

  def multiplyVector[E: Field](lhs: Vector[E], rhs: Vector[E]): E = {
    require(lhs.size == rhs.size)
    lhs.zip(rhs).map { case (x, y) => Field.multiply(x, y) }.combineAll
  }
}
