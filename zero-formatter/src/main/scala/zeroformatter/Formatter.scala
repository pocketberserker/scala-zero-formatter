package zeroformatter

import java.nio.ByteBuffer
import shapeless._
import shapeless.ops.hlist._

trait ZeroFormattable {

  def length: Option[Int]
}

case class DeserializeResult[T](value: T, byteSize: Int)

abstract class Formatter[T] extends ZeroFormattable {

  def serialize(bytes: Array[Byte], offset: Int, value: T): (Array[Byte], Int)

  def deserialize(buf: ByteBuffer, offset: Int): DeserializeResult[T]
}

object Formatter {

  import BinaryUtil._

  implicit val boolFormatter: Formatter[Boolean] = new Formatter[Boolean] {
    override val length = Some(1)
    override def serialize(bytes: Array[Byte], offset: Int, value: Boolean) =
      (writeBool(bytes, offset, value), 1)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.get(offset) != 0, 1)
  }

  implicit val byteFormatter: Formatter[Byte] = new Formatter[Byte] {
    override val length = Some(1)
    override def serialize(bytes: Array[Byte], offset: Int, value: Byte) =
      (writeByte(bytes, offset, value), 1)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.get(offset), 1)
  }

  implicit val shortFormatter: Formatter[Short] = new Formatter[Short] {
    override val length = Some(2)
    override def serialize(bytes: Array[Byte], offset: Int, value: Short) =
      (writeShort(bytes, offset, value), 2)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.getShort(offset), 2)
  }

  implicit val intFormatter: Formatter[Int] = new Formatter[Int] {
    override val length = Some(4)
    override def serialize(bytes: Array[Byte], offset: Int, value: Int) =
      (writeInt(bytes, offset, value), 4)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.getInt(offset), 4)
  }

  implicit val longFormatter: Formatter[Long] = new Formatter[Long] {
    override val length = Some(8)
    override def serialize(bytes: Array[Byte], offset: Int, value: Long) =
      (writeLong(bytes, offset, value), 8)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.getLong(offset), 8)
  }

  implicit val floatFormatter: Formatter[Float] = new Formatter[Float] {
    override val length = Some(4)
    override def serialize(bytes: Array[Byte], offset: Int, value: Float) =
      intFormatter.serialize(bytes, offset, java.lang.Float.floatToIntBits(value))
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.getFloat(offset), 4)
  }

  implicit val doubleFormatter: Formatter[Double] = new Formatter[Double] {
    override val length = Some(8)
    override def serialize(bytes: Array[Byte], offset: Int, value: Double) =
      longFormatter.serialize(bytes, offset, java.lang.Double.doubleToLongBits(value))
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.getDouble(offset), 8)
  }

  implicit val charFormatter: Formatter[Char] = new Formatter[Char] {
    override val length = Some(2)
    override def serialize(bytes: Array[Byte], offset: Int, value: Char) =
      (writeChar(bytes, offset, value), 2)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      DeserializeResult(buf.getChar(offset), 2)
  }

  implicit val stringFormatter: Formatter[String] = new Formatter[String] {
    override val length = None
    override def serialize(bytes: Array[Byte], offset: Int, value: String) =
      writeString(bytes, offset, value)
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val (value, size) = readString(buf, offset)
      DeserializeResult(value, size)
    }
  }

  private[this] object flatten extends Poly1 {
    implicit def some[T] = at[Some[Index]]{
      case Some(index) => index.value
    }
  }

  private[this] object writeObject extends Poly2 {
    implicit def caseValueIndex[T](implicit F: Formatter[T]) =
      at[(Array[Byte], Int, Int), (T, Int)] {
        case ((bytes, offset, byteSize), (value, index)) =>
          val o = offset + byteSize
          val (bs, size) = F.serialize(bytes, o, value)
          intFormatter.serialize(bs, offset + 4 + 4 + 4 * index, o)
          (bs, offset, byteSize + size)
      }
  }

  private[this] object zero extends Poly0 {
    implicit def zero[T] = at[T](null.asInstanceOf[T])
  }

  private[this] object genFormatter extends Poly2 {
    implicit def gen[T, U <: HList](implicit F: Formatter[T]) =
      at[T, U]((_, acc) => F :: acc)
  }

  private[this] object readObject extends Poly2 {
    implicit def caseIndex[T, U <: HList] =
      at[(Formatter[T], Int), (ByteBuffer, Int, U)] {
        case ((formatter, index), (buf, offset, acc)) =>
          val o = intFormatter.deserialize(buf, offset + 4 + 4 + 4 * index).value
          (buf, offset, formatter.deserialize(buf, o).value :: acc)
      }
  }

  implicit def objectFormatter[
    A, B <: HList, C <: HList, D <: HList, E <: HList, F <: HList,
    G <: HList,
    H <: HList, I <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    index: Annotations.Aux[Index, A, C],
    filterIndex: Partition.Aux[C, None.type, D, E],
    flattener: Mapper.Aux[flatten.type, E, F],
    toIndexList: ToTraversable.Aux[F, List, Int],
    // serialize
    zipper : Zip.Aux[B :: F :: HNil, G],
    write: LeftFolder.Aux[G, (Array[Byte], Int, Int), writeObject.type, (Array[Byte], Int, Int)],
    // deserialize
    init: FillWith[zero.type, B],
    generator: RightFolder.Aux[B, HNil, genFormatter.type, H],
    formatterZipper : Zip.Aux[H :: F :: HNil, I],
    read: RightFolder.Aux[I, (ByteBuffer, Int, HNil), readObject.type, (ByteBuffer, Int, B)]
    ): Formatter[A] = {

    val indexes = Annotations[Index, A].apply().filterNot[None.type].map(flatten)
    val lastIndexOpt = indexes.toList.reduceOption(_ max _)
    val formattersWithIndex =
      HList.fillWith[B](zero)
        .foldRight(HNil: HNil)(genFormatter)
        .zip(indexes)

    new Formatter[A] {

      override val length = None

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        lastIndexOpt match {
          case None => intFormatter.serialize(bytes, offset, -1)
          case Some(lastIndex) =>
            val values = gen.to(value).zip(indexes)
            val bs = writeInt(bytes, offset + 1, lastIndex)
            // [byteSize:int(4)] + [lastIndex:int(4)] + [indexOffset...:int(4 * lastIndex)]
            val initbyteSize = 4 + 4 + ((lastIndex + 1) * 4)
            val (result, _, byteSize) = values.foldLeft((bs, offset, initbyteSize))(writeObject)
            (writeInt(result, offset, byteSize), byteSize)
        }
      }

      override def deserialize(buf: ByteBuffer, offset: Int) = {
        val byteSize = intFormatter.deserialize(buf, offset).value
        val (_, _, vs) = formattersWithIndex.foldRight((buf, offset, HNil: HNil))(readObject)
        DeserializeResult(gen.from(vs), byteSize)
      }
    }
  }
}
