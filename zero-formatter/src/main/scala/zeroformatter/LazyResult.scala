package zeroformatter

sealed abstract class LazyResult[A] { self =>

  def value: A

  def byteSize: Int

  def map[B](f: (=> A) => B): LazyResult[B] = LazyResult[B](f(value), byteSize)

  def copy(newSize: => Int): LazyResult[A] = new LazyResult[A] {
    override def value = self.value
    override val byteSize = newSize
  }
}

object LazyResult {
  def apply[A](v: => A, s: Int): LazyResult[A] = new LazyResult[A] {
    override def value = v
    override val byteSize = s
  }
  def strict[A](v: A, s: Int): LazyResult[A] = new LazyResult[A] {
    override val value = v
    override val byteSize = s
  }
}
