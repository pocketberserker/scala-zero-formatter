package zeroformatter

import java.time._
import scala.reflect.ClassTag
import spire.math.{UByte, UShort, UInt, ULong}
import BinaryUtil._

abstract class FormatterInstances2 {

  implicit val boolFormatter: Formatter[Boolean] = new Formatter[Boolean] {
    override val length = Some(1)
    override def serialize(bytes: Array[Byte], offset: Int, value: Boolean) =
      LazyResult.strict(writeBool(bytes, offset, value), 1)
    override def deserialize(decoder: Decoder) =
      decoder.getByte() match {
        case 1 => true
        case 0 => false
        case _ => throw FormatException(decoder.offset, "Invalid Boolean byte.")
      }
  }

  implicit val byteFormatter: Formatter[Byte] = new Formatter[Byte] {
    override val length = Some(1)
    override def serialize(bytes: Array[Byte], offset: Int, value: Byte) =
      LazyResult.strict(writeByte(bytes, offset, value), 1)
    override def deserialize(decoder: Decoder) =
      decoder.getByte()
  }

  implicit val shortFormatter: Formatter[Short] = new Formatter[Short] {
    override val length = Some(2)
    override def serialize(bytes: Array[Byte], offset: Int, value: Short) =
      LazyResult.strict(writeShort(bytes, offset, value), 2)
    override def deserialize(decoder: Decoder) =
      decoder.getShort()
  }

  implicit val intFormatter: Formatter[Int] = new Formatter[Int] {
    override val length = Some(4)
    override def serialize(bytes: Array[Byte], offset: Int, value: Int) =
      LazyResult.strict(writeInt(bytes, offset, value), 4)
    override def deserialize(decoder: Decoder) =
      decoder.getInt()
  }

  implicit val longFormatter: Formatter[Long] = new Formatter[Long] {
    override val length = Some(8)
    override def serialize(bytes: Array[Byte], offset: Int, value: Long) =
      LazyResult.strict(writeLong(bytes, offset, value), 8)
    override def deserialize(decoder: Decoder) =
      decoder.getLong()
  }

  implicit val ubyteFormatter: Formatter[UByte] = byteFormatter.xmap(b => UByte(b), _.toByte)
  implicit val ushortFormatter: Formatter[UShort] = shortFormatter.xmap(s => UShort(s), _.toShort)
  implicit val uintFormatter: Formatter[UInt] = intFormatter.xmap(i => UInt(i), _.toInt)
  implicit val ulongFormatter: Formatter[ULong] = longFormatter.xmap(l => ULong(l), _.toLong)

  implicit val floatFormatter: Formatter[Float] = new Formatter[Float] {
    override val length = Some(4)
    override def serialize(bytes: Array[Byte], offset: Int, value: Float) =
      intFormatter.serialize(bytes, offset, java.lang.Float.floatToIntBits(value))
    override def deserialize(decoder: Decoder) =
      decoder.getFloat()
  }

  implicit val doubleFormatter: Formatter[Double] = new Formatter[Double] {
    override val length = Some(8)
    override def serialize(bytes: Array[Byte], offset: Int, value: Double) =
      longFormatter.serialize(bytes, offset, java.lang.Double.doubleToLongBits(value))
    override def deserialize(decoder: Decoder) =
      decoder.getDouble()
  }

  implicit val charFormatter: Formatter[Char] = new Formatter[Char] {
    override val length = Some(2)
    override def serialize(bytes: Array[Byte], offset: Int, value: Char) =
      LazyResult.strict(writeChar(bytes, offset, value), 2)
    override def deserialize(decoder: Decoder) =
      decoder.getChar()
  }

  implicit val stringFormatter: Formatter[String] = new Formatter[String] {
    override val length = None
    override def serialize(bytes: Array[Byte], offset: Int, value: String) =
      writeString(bytes, offset, value)
    override def deserialize(decoder: Decoder) =
      readString(decoder)
  }

  implicit val durationFormatter: Formatter[Duration] = new Formatter[Duration] {
    override val length = Some(12)
    override def serialize(bytes: Array[Byte], offset: Int, value: Duration) =
      LazyResult(intFormatter.serialize(longFormatter.serialize(bytes, offset, value.getSeconds).value, offset + 8, value.getNano).value, 12)
    override def deserialize(decoder: Decoder) = {
      val second = longFormatter.deserialize(decoder)
      val nano = intFormatter.deserialize(decoder)
      Duration.ofSeconds(second, nano)
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

    override def deserialize(decoder: Decoder) = {
      val length = intFormatter.deserialize(decoder)
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset, "Invalid Array length.")
      else {
        val array = new Array[T](length)
        var i = 0
        while(i < length) {
          array(i) = F.deserialize(decoder)
          i += 1
        }
        array
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

    override def deserialize(decoder: Decoder) = {
      val length = decoder.getInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset, "Invalid List length.")
      else {
        var list: List[A] = Nil
        var i = 0
        while(i < length) {
          list ::= F.deserialize(decoder)
          i += 1
        }
        list.reverse
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
      override def deserialize(decoder: Decoder) =
        (A1.deserialize(decoder), A2.deserialize(decoder))
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

      override def deserialize(decoder: Decoder) = {
        val length = decoder.getInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset, "Invalid Map length.")
        else {
          var i = 0
          val builder = Map.newBuilder[K, V]
          while(i < length) {
            builder += F.deserialize(decoder)
            i += 1
          }
          builder.result()
        }
      }
    }
  }
}

abstract class FormatterInstances1 extends FormatterInstances2 {

  implicit def optionFormatter[T](implicit F: Formatter[T]): Formatter[Option[T]] = new Formatter[Option[T]] {
    override val length = F.length

    override val default = None

    override def serialize(bytes: Array[Byte], offset: Int, value: Option[T]) = value match {
      case Some(v) => F.serialize(bytes, offset, v)
      case None => intFormatter.serialize(bytes, offset, -1)
    }

    override def deserialize(decoder: Decoder) = {
      val r = decoder.getInt()
      if(r == -1) None
      else if(r < -1) throw FormatException(decoder.offset, "Invalid length or byte size.")
      else {
        decoder.offset -= 4
        Some(F.deserialize(decoder))
      }
    }
  }
}

abstract class FormatterInstances0 extends FormatterInstances1 {

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

    override def deserialize(decoder: Decoder) = {
      val r = boolFormatter.deserialize(decoder)
      if(r) Some(F.deserialize(decoder))
      else None
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
