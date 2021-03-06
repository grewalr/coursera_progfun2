package calculator

object Polynomial
{
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] =
  {
    // ax² + bx + c
    // b² - 4ac
    Signal
    {
      val x = a()
      val y = b()
      val z = c()
      (y * y) - (4 * x * z)
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
  {
    // (-b ± √Δ) / 2a
    Signal
    {
      val x = a()
      val y = b()
      val z = c()
      val d = delta()

      if(d > 0)
      {
        Set(
          ((-y) + Math.sqrt(d)) / (2 * x),
          ((-y) - Math.sqrt(d)) / (2 * x)
        )
      }
      else Set(0.0, 0.0)
    }
  }
}
