object SumOfMultiples extends App {

  def sum(factors: Set[Int], limit: Int): Int = {

    var uniqueFactors = Set.empty[Int]

    for ( number <- factors ) findFactorsToLimit(number)

    def findFactorsToLimit(n: Int): Unit = {
      for (i <- 1 until limit if i % n == 0) uniqueFactors += i
    }

    uniqueFactors.foldLeft(0)(_ + _)
  }
}

