/**
 * Exercise 2
 * Implement the variance function in terms of flatMap .
 * If the mean of a sequence is m ,
 * the variance is the mean of math.pow(x - m, 2)
 * or each element x in the sequence.
 * */
def variance(xs: Seq[Double]): Option[Double] = {
  //calculates the mean of a sequence
  def mean(xs: Seq[Double]): Option[Double]= {
    if(xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }
  /**
   * @m mean
   * @x element of xs
   * variance = Math.pow(x-m,2)
   * How it works:
   * Apply mean to each element of the sequence (list of m's)
   * once I get that list I apply mean to xs and use the m's
   *
   * */
  mean(xs).flatMap(m=>mean(xs.map(x=>Math.pow(x-m,2))))
}
