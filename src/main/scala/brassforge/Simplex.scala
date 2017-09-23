// Author: Brian Forbes
package brassforge

//Arrays: A(ROW)(COLUMN)
class Simplex {

  // Input: Simplex Tableau in SEF
  // Output: Vector x: Optimal values of the input variables, and value
  def calculate(A: Array[Array[Double]]): (Vector[Double], Double) =
    {
      (Vector[Double](), 0.0)
    }

  def matrixRowAdd(A: Array[Double], B: Array[Double], c: Double) =
    (A zip B).map { case (a, b) => a + c*b }

  def pivot(T: Array[Array[Double]], r: Int, c: Int): Array[Array[Double]] =
  {
    val pivr = T(r).map(_ / T(r)(c))

    (T zipWithIndex).map{
      case (_, r) => pivr
      case (row, _) => matrixRowAdd(row, pivr, row(c))
    }
  }

  def firstNegativeColumn(T: Array[Array[Double]]): Int =
    T(0).indexWhere(_ < 0)

  def minRatioTest(T: Array[Array[Double]]): Int // index of row to pivot on (min ratio of last/i

}
