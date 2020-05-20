import examples.chapter4_error_handling._
import examples.chapter4_error_handling.Option._

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))


variance(List(1, 3, 5))   // Some(2.6666666666666665)