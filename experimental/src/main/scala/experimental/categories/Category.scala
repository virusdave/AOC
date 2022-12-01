package experimental
package categories

trait Arrow[-A, +B] {
  final type From = A
  final type To = B
  type Data
}

object Arrow {
  def fromFunction[A, B](fn: A => B): FunctionArrow[A, B] = FunctionArrow(fn)
}

trait DataArrow[-A, +B, +D] extends Arrow[A, B] {
  override final type Data = D
  def data: Data
}

case class FunctionArrow[-A, +B](private[this] val ab: A => B) extends DataArrow[A, B, A => B] {
  override def data: A => B = ab
  def apply(a: A): B = data(a)
  def compose[C](bc: B => C): FunctionArrow[A, C] = FunctionArrow(ab andThen bc)
}

abstract class Category[O, A <: Arrow[O, O]] {
  type Objects = O
  type Arrows = A
  def identity(o: O): A
  def compose[AA, BB, CC](l: Arrow[AA, BB], r: Arrow[BB, CC]): Arrow[AA, CC]
}