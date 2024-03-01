package hw4


// Implement the matrix multiplication model
object MatMulModel {

  // Function to perform matrix multiplication
  def multiply(a: Seq[Seq[Int]], b: Seq[Seq[Int]], p: MatMulParams): Seq[Seq[Int]] = {
    require(a.head.size == p.k, "A's column count must match k")
    require(b.size == p.k, "B's row count must match k")
    require(b.head.size == p.n, "B's column count must match n")

    // Using immutable data structures and functional programming principles
    val result = for (i <- a.indices) yield {
      for (j <- b.head.indices) yield {
        (for (k <- a.head.indices) yield a(i)(k) * b(k)(j)).sum
      }
    }
    result
  }
}

// Example usage
object MatMulExample extends App {
  // Define matrix A and B
  val a: Seq[Seq[Int]] = Seq(Seq(1, 2), Seq(3, 4))
  val b: Seq[Seq[Int]] = Seq(Seq(2, 0), Seq(1, 2))

  // Define parameters for matrix multiplication
  val params = MatMulParams(m = 2, k = 2, n = 2)

  // Perform matrix multiplication
  val result = MatMulModel.multiply(a, b, params)

  // Print the result
  println(result.map(_.mkString(" ")).mkString("\n"))
}
