package zeroformatter

import java.nio.ByteBuffer
import java.time._
import scala.reflect.ClassTag
import spire.math.{UByte, UShort, UInt, ULong}
import shapeless._
import shapeless.ops.hlist._
import BinaryUtil._

trait ZeroFormattable {

  def length: Option[Int]
}

abstract class Formatter[T] extends ZeroFormattable { self =>

  def default: T = null.asInstanceOf[T]

  def serialize(bytes: Array[Byte], offset: Int, value: T): LazyResult[Array[Byte]]

  def deserialize(buf: ByteBuffer, offset: Int): LazyResult[T]

  def xmap[U](f: T => U, g: U => T): Formatter[U] = new Formatter[U] {
    override def length = self.length
    override def default = f(self.default)
    override def serialize(bytes: Array[Byte], offset: Int, value: U) =
      self.serialize(bytes, offset, g(value))
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = self.deserialize(buf, offset)
      LazyResult(f(r.value), r.byteSize)
    }
  }
}

object Formatter extends FormatterInstances {

  def apply[T](implicit F: Formatter[T]): Formatter[T] = F

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
          val r = F.serialize(bytes, o, value)
          val r2 = intFormatter.serialize(r.value, offset + 4 + 4 + 4 * index, o)
          (r2.value, offset, byteSize + r.byteSize)
      }
  }

  private[zeroformatter] object zero extends Poly0 {
    implicit def zero[T] = at[T](null.asInstanceOf[T])
  }

  private[this] object genObjectFormatter extends Poly2 {
    implicit def gen[T, U <: HList](implicit F: Formatter[T]) =
      at[T, U]((_, acc) => F :: acc)
  }

  private[this] case class ReadObjectResult[T <: HList](buf: ByteBuffer, offset: Int, lastIndex: Int, value: T)

  private[this] object readObject extends Poly2 {
    implicit def read[T, U <: HList] =
      at[(Formatter[T], Int), ReadObjectResult[U]] {
        case ((formatter, index), acc) =>
          val v =
            if(index > acc.lastIndex) formatter.default
            else {
              val o = intFormatter.deserialize(acc.buf, acc.offset + 4 + 4 + 4 * index).value
              if(o == 0) formatter.default
              else formatter.deserialize(acc.buf, o).value
            }
          acc.copy(value = v :: acc.value)
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
    read: RightFolder.Aux[I, ReadObjectResult[HNil], readObject.type, ReadObjectResult[B]]
    ): Formatter[A] = {

    val indexes = Annotations[Index, A].apply().filterNot[None.type].map(flatten)
    val lastIndex = indexes.toList.reduceOption(_ max _).getOrElse(throw FormatException(0, "Target object does not have index fields."))
    val formattersWithIndex =
      HList.fillWith[B](zero)
        .foldRight(HNil: HNil)(genObjectFormatter)
        .zip(indexes)

    new Formatter[A] {

      override val length = None

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        val values = gen.to(value).zip(indexes)
        val bs = writeInt(bytes, offset + 4, lastIndex)
        // [byteSize:int(4)] + [lastIndex:int(4)] + [indexOffset...:int(4 * lastIndex)]
        val initbyteSize = 4 + 4 + ((lastIndex + 1) * 4)
        val (result, _, byteSize) = values.foldLeft((bs, offset, initbyteSize))(writeObject)
        LazyResult(writeInt(result, offset, byteSize), byteSize)
      }

      override def deserialize(buf: ByteBuffer, offset: Int) = {
        val byteSize = intFormatter.deserialize(buf, offset).value
        if(byteSize == -1) LazyResult(null.asInstanceOf[A], byteSize)
        else if(byteSize < -1) throw FormatException(offset, "Invalid byte size.")
        else {
          val li = intFormatter.deserialize(buf, offset + 4).value
          val result = formattersWithIndex.foldRight(ReadObjectResult(buf, offset, li, HNil: HNil))(readObject)
          LazyResult(gen.from(result.value), byteSize)
        }
      }
    }
  }

  private[this] object writeEnum extends Poly2 {
    implicit def default[T <: Enum[_]] =
      at[(LazyResult[Array[Byte]], Int), T] {
        case ((result, offset), value) =>
          (value.serialize(result.value, offset), offset)
      }
  }

  private[this] case class EnumFormatterResult[C <: Coproduct, H <: HList](fake: C, value: H)

  private[this] object genEnumFormatter extends Poly2 {
    implicit def gen[
      N <: Nat, C <: Coproduct, H <: HList, V <: Enum[_],
      K <: HList
    ](implicit
      a: ops.coproduct.At.Aux[C, N, V],
      gen: Generic.Aux[V, K],
      init: FillWith[zero.type, K]
    ) =
      at[N, EnumFormatterResult[C, H]]{ case (_, acc) =>
        val kf = gen.from(HList.fillWith[K](zero))
        EnumFormatterResult(acc.fake, kf :: acc.value)
      }
  }

  private[this] case class ReadEnumResult[T](buf: ByteBuffer, offset: Int, value: Option[T])

  private[this] object readEnum extends Poly2 {
    implicit def read[V <: Enum[_], U <: Enum[_]] =
      at[ReadEnumResult[LazyResult[U]], V] {
        case (acc, f) => acc.value match {
          case Some(_) => acc
          case None =>
            f.check(acc.buf, acc.offset) match {
              case Some(byteSize) =>
                acc.copy(value = Some(LazyResult(f.asInstanceOf[U], byteSize)))
              case None => acc
            }
        }
      }
  }

  implicit def enumFormatter[
    A <: Enum[_], B <: Coproduct,
    C <: Nat, D <: HList, E <: HList, F <: Enum[_], G <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    length: ops.coproduct.Length.Aux[B, C],
    range: ops.nat.Range.Aux[nat._0, C, D],
    generator: RightFolder.Aux[D, EnumFormatterResult[B, HNil], genEnumFormatter.type, EnumFormatterResult[B, E]],
    isHCons: IsHCons.Aux[E, F, G],
    // serialize
    write: ops.coproduct.LeftFolder.Aux[B, (LazyResult[Array[Byte]], Int), writeEnum.type, (LazyResult[Array[Byte]], Int)],
    // deserialize
    read: LeftFolder.Aux[E, ReadEnumResult[LazyResult[A]], readEnum.type, ReadEnumResult[LazyResult[A]]]
  ): Formatter[A] = {

    val fs =
      range()
        .foldRight(EnumFormatterResult(null.asInstanceOf[B], HNil: HNil))(genEnumFormatter)
        .value

    new Formatter[A] {

      override val length = fs.head.length

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        val values = gen.to(value)
        values.foldLeft((LazyResult(bytes, 0), offset))(writeEnum)
          ._1
      }

      override def deserialize(buf: ByteBuffer, offset: Int) = {
        fs.foldLeft(ReadEnumResult(buf, offset, None: Option[LazyResult[A]]))(readEnum)
          .value
          .getOrElse(throw FormatException(offset, "EnumFormatter could not deserialize Enum label."))
      }
    }
  }

  private[this] object writeStruct extends Poly2 {
    implicit def write[T](implicit F: Formatter[T]) =
      at[(Array[Byte], Int, Int), T] {
        case ((bytes, offset, byteSize), value) =>
          val r = F.serialize(bytes, offset, value)
          (r.value, offset + r.byteSize, byteSize + r.byteSize)
      }
  }

  private[this] case class ReadStructResult[T <: HList](buf: ByteBuffer, offset: Int, value: T, byteSize: Int)

  private[this] object readStruct extends Poly2 {
    implicit def read[T, U <: HList] =
      at[ReadStructResult[U], Formatter[T]] {
        case (acc, formatter) =>
          val v = formatter.deserialize(acc.buf, acc.offset + acc.byteSize)
          acc.copy(value = v.value :: acc.value, byteSize = acc.byteSize + v.byteSize)
      }
  }

  implicit def structFormatter[
    A <: Struct, B <: HList, C <: HList, D <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    // serialize
    write: LeftFolder.Aux[B, (Array[Byte], Int, Int), writeStruct.type, (Array[Byte], Int, Int)],
    // deserialize
    init: FillWith[zero.type, B],
    generator: RightFolder.Aux[B, HNil, genObjectFormatter.type, C],
    read: LeftFolder.Aux[C, ReadStructResult[HNil], readStruct.type, ReadStructResult[D]],
    reverse: Reverse.Aux[D, B]
    ): Formatter[A] = {

    val formatters =
      HList.fillWith[B](zero)
        .foldRight(HNil: HNil)(genObjectFormatter)

    new Formatter[A] {

      override val length = None

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        val (result, _, byteSize) =
          gen.to(value).foldLeft((bytes, offset, 0))(writeStruct)
        LazyResult(result, byteSize)
      }

      override def deserialize(buf: ByteBuffer, offset: Int) = {
        val result =
          formatters.foldLeft(ReadStructResult(buf, offset, HNil: HNil, 0))(readStruct)
        LazyResult(gen.from(result.value.reverse), result.byteSize)
      }
    }
  }

  implicit def structOptionFormatter[A <: Struct : Formatter]: Formatter[Option[A]] =
    nullableFormatter[A]
}

abstract class FormatterInstances1 {

  implicit val boolFormatter: Formatter[Boolean] = new Formatter[Boolean] {
    override val length = Some(1)
    override def serialize(bytes: Array[Byte], offset: Int, value: Boolean) =
      LazyResult.strict(writeBool(bytes, offset, value), 1)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(
        buf.get(offset) match {
          case 1 => true
          case 0 => false
          case _ => throw FormatException(offset, "Invalid Boolean byte.")
        },
        1
      )
  }

  implicit val byteFormatter: Formatter[Byte] = new Formatter[Byte] {
    override val length = Some(1)
    override def serialize(bytes: Array[Byte], offset: Int, value: Byte) =
      LazyResult.strict(writeByte(bytes, offset, value), 1)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(buf.get(offset), 1)
  }

  implicit val shortFormatter: Formatter[Short] = new Formatter[Short] {
    override val length = Some(2)
    override def serialize(bytes: Array[Byte], offset: Int, value: Short) =
      LazyResult.strict(writeShort(bytes, offset, value), 2)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(buf.getShort(offset), 2)
  }

  implicit val intFormatter: Formatter[Int] = new Formatter[Int] {
    override val length = Some(4)
    override def serialize(bytes: Array[Byte], offset: Int, value: Int) =
      LazyResult.strict(writeInt(bytes, offset, value), 4)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(buf.getInt(offset), 4)
  }

  implicit val longFormatter: Formatter[Long] = new Formatter[Long] {
    override val length = Some(8)
    override def serialize(bytes: Array[Byte], offset: Int, value: Long) =
      LazyResult.strict(writeLong(bytes, offset, value), 8)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(buf.getLong(offset), 8)
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
      LazyResult.strict(buf.getFloat(offset), 4)
  }

  implicit val doubleFormatter: Formatter[Double] = new Formatter[Double] {
    override val length = Some(8)
    override def serialize(bytes: Array[Byte], offset: Int, value: Double) =
      longFormatter.serialize(bytes, offset, java.lang.Double.doubleToLongBits(value))
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(buf.getDouble(offset), 8)
  }

  implicit val charFormatter: Formatter[Char] = new Formatter[Char] {
    override val length = Some(2)
    override def serialize(bytes: Array[Byte], offset: Int, value: Char) =
      LazyResult.strict(writeChar(bytes, offset, value), 2)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      LazyResult.strict(buf.getChar(offset), 2)
  }

  implicit val stringFormatter: Formatter[String] = new Formatter[String] {
    override val length = None
    override def serialize(bytes: Array[Byte], offset: Int, value: String) =
      writeString(bytes, offset, value)
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      readString(buf, offset)
    }
  }

  implicit val durationFormatter: Formatter[Duration] = new Formatter[Duration] {
    override val length = Some(12)
    override def serialize(bytes: Array[Byte], offset: Int, value: Duration) =
      LazyResult(intFormatter.serialize(longFormatter.serialize(bytes, offset, value.getSeconds).value, offset + 8, value.getNano).value, 12)
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val second = longFormatter.deserialize(buf, offset).value
      val nano = intFormatter.deserialize(buf, offset + 8).value
      LazyResult(Duration.ofSeconds(second, nano), 12)
    }
  }

  implicit def arrayFormatter[T: ClassTag](implicit F: Formatter[T]): Formatter[Array[T]] = new Formatter[Array[T]] {
    override val length = None

    override def serialize(bytes: Array[Byte], offset: Int, value: Array[T]) =
      if(value == null) {
        intFormatter.serialize(bytes, offset, -1)
      }
      else {
        val length = value.length
        var bs = F.length.map(l => ensureCapacity(bytes, offset, 4 + l * length)).getOrElse(bytes)


        val lr = intFormatter.serialize(bs, offset, length)
        bs = lr.value

        var byteSize = lr.byteSize
        var i = 0
        while(i < length) {
          val r = F.serialize(bs, offset + byteSize, value(i))
          bs = r.value
          byteSize += r.byteSize
          i += 1
        }
        LazyResult.strict(bs, byteSize)
      }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val length = intFormatter.deserialize(buf, offset).value
      if(length == -1) LazyResult(null, 4)
      else if(length < -1) throw FormatException(offset, "Invalid Array length.")
      else {
        val array = new Array[T](length)
        var byteSize = 4
        var i = 0
        while(i < length) {
          val r = F.deserialize(buf, offset + byteSize)
          array(i) = r.value
          byteSize += r.byteSize
          i += 1
        }
        LazyResult.strict(array, byteSize)
      }
    }
  }

  implicit def listFormatter[A](implicit F: Formatter[A]): Formatter[List[A]] = new Formatter[List[A]] {
    override val length = None

    override def serialize(bytes: Array[Byte], offset: Int, value: List[A]) =
      if(value == null) {
        intFormatter.serialize(bytes, offset, -1)
      }
      else {
        val length = value.length
        var bs = F.length.map(l => ensureCapacity(bytes, offset, 4 + l * length)).getOrElse(bytes)

        val lr = intFormatter.serialize(bs, offset, length)
        bs = lr.value

        var x = value
        var byteSize = lr.byteSize
        while(x ne Nil) {
          val r = F.serialize(bs, offset + byteSize, x.head)
          bs = r.value
          byteSize += r.byteSize
          x = x.tail
        }
        LazyResult.strict(bs, byteSize)
      }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val length = intFormatter.deserialize(buf, offset).value
      if(length == -1) LazyResult(null, 4)
      else if(length < -1) throw FormatException(offset, "Invalid Array length.")
      else {
        var list: List[A] = Nil
        var byteSize = 4
        var i = 0
        while(i < length) {
          val r = F.deserialize(buf, offset + byteSize)
          list ::= r.value
          byteSize += r.byteSize
          i += 1
        }
        LazyResult.strict(list.reverse, byteSize)
      }
    }
  }

  implicit def tuple2Formatter[A1, A2](implicit
    A1: Formatter[A1],
    A2: Formatter[A2]
  ): Formatter[(A1, A2)] =
    new Formatter[(A1, A2)] {
       override val length = (A1.length, A2.length) match {
         case (Some(l1), Some(l2)) => Some(l1 + l2)
         case _ => None
      }
      override def serialize(bytes: Array[Byte], offset: Int, value: (A1, A2)) = {
        val r1 = A1.serialize(bytes, offset, value._1)
        val r2 = A2.serialize(r1.value, offset + r1.byteSize, value._2)
        r2.copy(r1.byteSize + r2.byteSize)
      }
      override def deserialize(buf: ByteBuffer, offset: Int) = {
        val r1 = A1.deserialize(buf, offset)
        val r2 = A2.deserialize(buf, offset + r1.byteSize)
        LazyResult((r1.value, r2.value), r1.byteSize + r2.byteSize)
      }
    }

  implicit def mapFormatter[K: Formatter, V: Formatter]: Formatter[Map[K, V]] = {
    val F = Formatter[(K, V)]
    new Formatter[Map[K, V]] {
      override val length = None

      override def serialize(bytes: Array[Byte], offset: Int, value: Map[K, V]) =
        if(value == null) {
          intFormatter.serialize(bytes, offset, -1)
        }
        else {
          val length = value.size
          val bs = F.length.map(l => ensureCapacity(bytes, offset, 4 + l * length)).getOrElse(bytes)

          value.foldLeft(intFormatter.serialize(bs, offset, length)){ case (acc, kv) =>
            val r = F.serialize(acc.value, offset + acc.byteSize, kv)
            LazyResult.strict(r.value, acc.byteSize + r.byteSize)
          }
        }

      override def deserialize(buf: ByteBuffer, offset: Int) = {
        val length = intFormatter.deserialize(buf, offset).value
        if(length == -1) LazyResult(null, 4)
        else if(length < -1) throw FormatException(offset, "Invalid Map length.")
        else (0 to length - 1).foldLeft(LazyResult(Map[K, V](), 4)){ case (res, _) =>
          val r = F.deserialize(buf, offset + res.byteSize)
          LazyResult.strict(res.value + r.value, res.byteSize + r.byteSize)
        }
      }
    }
  }
}

abstract class FormatterInstances0 extends FormatterInstances1 {

  implicit def optionFormatter[T](implicit F: Formatter[T]): Formatter[Option[T]] = new Formatter[Option[T]] {
    override val length = F.length

    override val default = None

    override def serialize(bytes: Array[Byte], offset: Int, value: Option[T]) = value match {
      case Some(v) => F.serialize(bytes, offset, v)
      case None => intFormatter.serialize(bytes, offset, -1)
    }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = intFormatter.deserialize(buf, offset)
      if(r.value == -1) LazyResult(None, r.byteSize)
      else if(r.value < -1) throw FormatException(offset, "Invalid length or byte size.")
      else {
        val result = F.deserialize(buf, offset)
        LazyResult(Some(result.value), result.byteSize)
      }
    }
  }
}

abstract class FormatterInstances extends FormatterInstances0 {

  def nullableFormatter[T](implicit F: Formatter[T]): Formatter[Option[T]] = new Formatter[Option[T]] {
    override val length = F.length.map(_ + 1)

    override val default = None

    override def serialize(bytes: Array[Byte], offset: Int, value: Option[T]) = value match {
      case Some(v) =>
        val bs = F.length.map(l => ensureCapacity(bytes, offset, l)).getOrElse(bytes)
        val result = F.serialize(boolFormatter.serialize(bs, offset, true).value, offset + 1, v)
        result.copy(result.byteSize + 1)
      case None => boolFormatter.serialize(bytes, offset, false)
    }

    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = boolFormatter.deserialize(buf, offset)
      if(r.value) {
        val result = F.deserialize(buf, offset + 1)
        LazyResult(Some(result.value), r.byteSize + result.byteSize)
      }
      else LazyResult(None, r.byteSize)
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

  implicit val stringOptionFormatter: Formatter[Option[String]] =
    stringFormatter.xmap(Option[String](_), _.orNull)
}
