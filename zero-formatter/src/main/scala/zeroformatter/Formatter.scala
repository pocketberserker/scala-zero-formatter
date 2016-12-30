package zeroformatter

import java.nio.ByteBuffer
import java.time._
import spire.math.{UByte, UShort, UInt, ULong}
import shapeless._
import shapeless.ops.hlist._
import BinaryUtil._

trait ZeroFormattable {

  def length: Option[Int]
}

case class DeserializeResult[T](value: T, byteSize: Int)

abstract class Formatter[T] extends ZeroFormattable { self =>

  def serialize(bytes: Array[Byte], offset: Int, value: T): (Array[Byte], Int)

  def deserialize(buf: ByteBuffer, offset: Int): DeserializeResult[T]

  def xmap[U](f: T => U, g: U => T): Formatter[U] = new Formatter[U] {
    override def length = self.length
    override def serialize(bytes: Array[Byte], offset: Int, value: U) =
      self.serialize(bytes, offset, g(value))
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = self.deserialize(buf, offset)
      DeserializeResult(f(r.value), r.byteSize)
    }
  }
}

object Formatter extends FormatterInstances {

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

  private[zeroformatter] object zero extends Poly0 {
    implicit def zero[T] = at[T](null.asInstanceOf[T])
  }

  private[this] object genObjectFormatter extends Poly2 {
    implicit def gen[T, U <: HList](implicit F: Formatter[T]) =
      at[T, U]((_, acc) => F :: acc)
  }

  private[this] object readObject extends Poly2 {
    implicit def read[T, U <: HList] =
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
    generator: RightFolder.Aux[B, HNil, genObjectFormatter.type, H],
    formatterZipper : Zip.Aux[H :: F :: HNil, I],
    read: RightFolder.Aux[I, (ByteBuffer, Int, HNil), readObject.type, (ByteBuffer, Int, B)]
    ): Formatter[A] = {

    val indexes = Annotations[Index, A].apply().filterNot[None.type].map(flatten)
    val lastIndexOpt = indexes.toList.reduceOption(_ max _)
    val formattersWithIndex =
      HList.fillWith[B](zero)
        .foldRight(HNil: HNil)(genObjectFormatter)
        .zip(indexes)

    new Formatter[A] {

      override val length = None

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        lastIndexOpt match {
          case None => intFormatter.serialize(bytes, offset, -1)
          case Some(lastIndex) =>
            val values = gen.to(value).zip(indexes)
            val bs = writeInt(bytes, offset + 4, lastIndex)
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

abstract class FormatterInstances1 {

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

  implicit val ubyteFormatter: Formatter[UByte] = byteFormatter.xmap(b => UByte(b), _.toByte)
  implicit val ushortFormatter: Formatter[UShort] = shortFormatter.xmap(s => UShort(s), _.toShort)
  implicit val uintFormatter: Formatter[UInt] = intFormatter.xmap(i => UInt(i), _.toInt)
  implicit val ulongFormatter: Formatter[ULong] = longFormatter.xmap(l => ULong(l), _.toLong)

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

  implicit val durationFormatter: Formatter[Duration] = new Formatter[Duration] {
    override val length = Some(12)
    override def serialize(bytes: Array[Byte], offset: Int, value: Duration) =
      (intFormatter.serialize(longFormatter.serialize(bytes, offset, value.getSeconds)._1, offset + 8, value.getNano)._1, 12)
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val second = longFormatter.deserialize(buf, offset).value
      val nano = intFormatter.deserialize(buf, offset + 8).value
      DeserializeResult(Duration.ofSeconds(second, nano), 12)
    }
  }

  implicit def arrayFormatter[T: scala.reflect.ClassTag](implicit F: Formatter[T]): Formatter[Array[T]] = new Formatter[Array[T]] {
    override val length = None

    override def serialize(bytes: Array[Byte], offset: Int, value: Array[T]) =
      if(value == null) {
        intFormatter.serialize(bytes, offset, -1)
      }
      else {
        val length = value.length
        val bs = F.length.map(l => ensureCapacity(bytes, offset, 4 + l * length)).getOrElse(bytes)

        val (result, byteSize) = value.foldLeft((bs, 4)){ case ((acc, size), v) =>
          val (r, s) = F.serialize(acc, offset + size, v)
          (r, size + s)
        }

        intFormatter.serialize(result, offset, length)
        (result, byteSize)
      }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val length = intFormatter.deserialize(buf, offset).value
      if(length == -1) DeserializeResult(null.asInstanceOf[Array[T]], 4)
      else (0 to length - 1).foldLeft(DeserializeResult(new Array[T](length), 4)){ case (res, i) =>
        val r = F.deserialize(buf, offset + res.byteSize)
        res.value(i) = r.value
        res.copy(byteSize = res.byteSize + r.byteSize)
      }
    }
  }
}

abstract class FormatterInstances0 extends FormatterInstances1 {

  implicit def optionFormatter[T](implicit F: Formatter[T]): Formatter[Option[T]] = new Formatter[Option[T]] {
    override val length = F.length

    override def serialize(bytes: Array[Byte], offset: Int, value: Option[T]) = value match {
      case Some(v) => F.serialize(bytes, offset, v)
      case None => intFormatter.serialize(bytes, offset, -1)
    }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = intFormatter.deserialize(buf, offset)
      if(r.value == -1) DeserializeResult(None, r.byteSize)
      else {
        val result = F.deserialize(buf, offset)
        DeserializeResult(Some(result.value), result.byteSize)
      }
    }
  }
}

abstract class FormatterInstances extends FormatterInstances0 {

  def nullableFormatter[T](implicit F: Formatter[T]): Formatter[Option[T]] = new Formatter[Option[T]] {
    override val length = F.length.map(_ + 1)

    override def serialize(bytes: Array[Byte], offset: Int, value: Option[T]) = value match {
      case Some(v) =>
        val bs = F.length.map(l => ensureCapacity(bytes, offset, l)).getOrElse(bytes)
        val (result, s) = F.serialize(boolFormatter.serialize(bs, offset, true)._1, offset + 1, v)
        (result, s + 1)
      case None => boolFormatter.serialize(bytes, offset, false)
    }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = boolFormatter.deserialize(buf, offset)
      if(r.value) {
        val result = F.deserialize(buf, offset + 1)
        DeserializeResult(Some(result.value), r.byteSize + result.byteSize)
      }
      else DeserializeResult(None, r.byteSize)
    }
  }

  implicit val boolOptionFormatter: Formatter[Option[Boolean]] = nullableFormatter[Boolean]
  implicit val byteOptionFormatter: Formatter[Option[Byte]] = nullableFormatter[Byte]
  implicit val shortOptionFormatter: Formatter[Option[Short]] = nullableFormatter[Short]
  implicit val intOptionFormatter: Formatter[Option[Int]] = nullableFormatter[Int]
  implicit val longOptionFormatter: Formatter[Option[Long]] = nullableFormatter[Long]
  implicit val ubyteOptionFormatter: Formatter[Option[UByte]] = nullableFormatter[UByte]
  implicit val ushortOptionFormatter: Formatter[Option[UShort]] = nullableFormatter[UShort]
  implicit val uintOptionFormatter: Formatter[Option[UInt]] = nullableFormatter[UInt]
  implicit val ulongOptionFormatter: Formatter[Option[ULong]] = nullableFormatter[ULong]
  implicit val floatOptionFormatter: Formatter[Option[Float]] = nullableFormatter[Float]
  implicit val doubleOptionFormatter: Formatter[Option[Double]] = nullableFormatter[Double]
  implicit val charOptionFormatter: Formatter[Option[Char]] = nullableFormatter[Char]
  implicit val durationOptionFormatter: Formatter[Option[Duration]] = nullableFormatter[Duration]
}
