package u06lab.solutions

/*
 * 1) Implement trait Functions with an object FunctionsImpl such that
 *    the code in TryFunctions works correctly.
 */
trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

object FunctionsImpl extends Functions:
  import Combiners.given
  override def sum(a: List[Double]): Double = combine(a)
  override def concat(a: Seq[String]): String = combine(a)
  override def max(a: List[Int]): Int = combine(a)
  private def combine[A: Combiner](a: Seq[A]) =
    if a.isEmpty then summon[Combiner[A]].unit else a.reduce((a1, a2) => summon[Combiner[A]].combine(a1, a2))

/*
 * 2) To apply DRY principle at the best,
 *    note the three methods in Functions do something similar.
 *    Use the following approach:
 *    - find three implementations of Combiner that tell (for sum, concat and max) how
 *      to combine two elements, and what to return when the input list is empty
 *    - implement in FunctionsImpl a single method combiner that, other than
 *      the collection of A, takes a Combiner as input
 *    - implement the three methods by simply calling combiner
 *
 *    When all works, note we completely avoided duplications..
 */
trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object Combiners:

  given Combiner[String] with
    override def unit: String = ""
    override def combine(a: String, b: String): String = a ++ b

  given Combiner[Double] with
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a + b

  given Combiner[Int] with
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = if a > b then a else b
