package zeroformatter

import java.time._
import scala.reflect.ClassTag
import spire.math.{UByte, UShort, UInt, ULong}

abstract class FormatterInstances2 {

  implicit val boolFormatter: Formatter[Boolean] = new Formatter[Boolean] {
    override val length = Some(1)
    override def serialize(encoder: Encoder, offset: Int, value: Boolean) =
      encoder.writeBool(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readBool()
  }

  implicit val byteFormatter: Formatter[Byte] = new Formatter[Byte] {
    override val length = Some(1)
    override def serialize(encoder: Encoder, offset: Int, value: Byte) =
      encoder.writeByte(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readByte()
  }

  implicit val shortFormatter: Formatter[Short] = new Formatter[Short] {
    override val length = Some(2)
    override def serialize(encoder: Encoder, offset: Int, value: Short) =
      encoder.writeShort(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readShort()
  }

  implicit val intFormatter: Formatter[Int] = new Formatter[Int] {
    override val length = Some(4)
    override def serialize(encoder: Encoder, offset: Int, value: Int) =
      encoder.writeInt(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readInt()
  }

  implicit val longFormatter: Formatter[Long] = new Formatter[Long] {
    override val length = Some(8)
    override def serialize(encoder: Encoder, offset: Int, value: Long) =
      encoder.writeLong(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readLong()
  }

  implicit val ubyteFormatter: Formatter[UByte] = byteFormatter.xmap(b => UByte(b), _.toByte)
  implicit val ushortFormatter: Formatter[UShort] = shortFormatter.xmap(s => UShort(s), _.toShort)
  implicit val uintFormatter: Formatter[UInt] = intFormatter.xmap(i => UInt(i), _.toInt)
  implicit val ulongFormatter: Formatter[ULong] = longFormatter.xmap(l => ULong(l), _.toLong)

  implicit val floatFormatter: Formatter[Float] = new Formatter[Float] {
    override val length = Some(4)
    override def serialize(encoder: Encoder, offset: Int, value: Float) =
      encoder.writeFloat(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readFloat()
  }

  implicit val doubleFormatter: Formatter[Double] = new Formatter[Double] {
    override val length = Some(8)
    override def serialize(encoder: Encoder, offset: Int, value: Double) =
      encoder.writeDouble(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readDouble()
  }

  implicit val charFormatter: Formatter[Char] = new Formatter[Char] {
    override val length = Some(2)
    override def serialize(encoder: Encoder, offset: Int, value: Char) =
      encoder.writeChar(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readChar()
  }

  implicit val stringFormatter: Formatter[String] = new Formatter[String] {
    override val length = None
    override def serialize(encoder: Encoder, offset: Int, value: String) =
      encoder.writeString(offset, value)
    override def deserialize(decoder: Decoder) =
      decoder.readString()
  }

  implicit val durationFormatter: Formatter[Duration] = new Formatter[Duration] {
    override val length = Some(12)
    override def serialize(encoder: Encoder, offset: Int, value: Duration) = {
      encoder.ensureCapacity(offset, 12)
      val r1 = encoder.writeLongUnsafe(offset, value.getSeconds)
      val r2 = encoder.writeIntUnsafe(offset + 8, value.getNano)
      r1 + r2
    }
    override def deserialize(decoder: Decoder) =
      Duration.ofSeconds(decoder.readLong(), decoder.readInt())
  }

  implicit def arrayFormatter[T: ClassTag](implicit F: Formatter[T]): Formatter[Array[T]] = new Formatter[Array[T]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[T]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        var byteSize =
          F.length.fold(encoder.writeInt(offset, length))(
            l => {
              encoder.ensureCapacity(offset, 4 + l * length)
              encoder.writeIntUnsafe(offset, length)
            }
          )

        var i = 0
        while(i < length) {
          byteSize += F.serialize(encoder, offset + byteSize, value(i))
          i += 1
        }
        byteSize
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
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

  implicit def listFormatter[A](implicit F: Formatter[A]): Formatter[List[A]] = {

    @annotation.tailrec
    def go(v: List[A], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + F.serialize(encoder, offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[A]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[A]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          F.length.fold(encoder.writeInt(offset, length))(
            l => {
              encoder.ensureCapacity(offset, 4 + l * length)
              encoder.writeIntUnsafe(offset, length)
            }
          )

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[A]()
          var i = 0
          while(i < length) {
            list += F.deserialize(decoder)
            i += 1
          }
          list.toList
        }
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
      override def serialize(encoder: Encoder, offset: Int, value: (A1, A2)) = {
        val r1 = A1.serialize(encoder, offset, value._1)
        val r2 = A2.serialize(encoder, offset + r1, value._2)
        r1 + r2
      }
      override def deserialize(decoder: Decoder) =
        (A1.deserialize(decoder), A2.deserialize(decoder))
    }

  implicit def mapFormatter[K: Formatter, V: Formatter]: Formatter[Map[K, V]] = {
    val F = Formatter[(K, V)]
    new Formatter[Map[K, V]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Map[K, V]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.size
          val byteSize =
            F.length.fold(encoder.writeInt(offset, length))(
              l => {
                encoder.ensureCapacity(offset, 4 + l * length)
                encoder.writeIntUnsafe(offset, length)
              }
            )

          value.foldLeft(byteSize){ case (acc, kv) =>
            acc + F.serialize(encoder, offset + acc, kv)
          }
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Map length($length).")
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

  implicit def vectorFormatter[T](implicit F: Formatter[T]): Formatter[Vector[T]] = {

    @annotation.tailrec
    def go(v: Vector[T], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + F.serialize(encoder, offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[T]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[T]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          F.length.fold(encoder.writeInt(offset, length))(
            l => {
              encoder.ensureCapacity(offset, 4 + l * length)
              encoder.writeIntUnsafe(offset, length)
            }
          )

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[T]
          var i = 0
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

    override def serialize(encoder: Encoder, offset: Int, value: Option[T]) = value match {
      case Some(v) => F.serialize(encoder, offset, v)
      case None => encoder.writeInt(offset, -1)
    }

    override def deserialize(decoder: Decoder) =
      Option[T](F.deserialize(decoder))
  }

  implicit val arrayByteFormatter: Formatter[Array[Byte]] = new Formatter[Array[Byte]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[Byte]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        encoder.ensureCapacity(offset, 4 + length)
        val byteSize = encoder.writeIntUnsafe(offset, length)

        byteSize + encoder.writeByteArrayUnsafe(offset + byteSize, value, 0, length)
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
      else {
        val array = new Array[Byte](length)
        var i = 0
        while(i < length) {
          array(i) = decoder.readByte()
          i += 1
        }
        array
      }
    }
  }

  implicit val arrayBoolFormatter: Formatter[Array[Boolean]] = new Formatter[Array[Boolean]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[Boolean]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        encoder.ensureCapacity(offset, 4 + length)
        var byteSize = encoder.writeIntUnsafe(offset, length)

        var i = 0
        while(i < length) {
          byteSize += encoder.writeBoolUnsafe(offset + byteSize, value(i))
          i += 1
        }
        byteSize
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
      else {
        val array = new Array[Boolean](length)
        var i = 0
        while(i < length) {
          array(i) = decoder.readBool()
          i += 1
        }
        array
      }
    }
  }

  implicit val arrayShortFormatter: Formatter[Array[Short]] = new Formatter[Array[Short]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[Short]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        encoder.ensureCapacity(offset, 4 + 2 * length)
        var byteSize = encoder.writeIntUnsafe(offset, length)

        var i = 0
        while(i < length) {
          byteSize += encoder.writeShortUnsafe(offset + byteSize, value(i))
          i += 1
        }
        byteSize
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
      else {
        val array = new Array[Short](length)
        var i = 0
        while(i < length) {
          array(i) = decoder.readShort()
          i += 1
        }
        array
      }
    }
  }

  implicit val arrayIntFormatter: Formatter[Array[Int]] = new Formatter[Array[Int]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[Int]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        encoder.ensureCapacity(offset, 4 + 4 * length)
        var byteSize = encoder.writeIntUnsafe(offset, length)

        var i = 0
        while(i < length) {
          byteSize += encoder.writeIntUnsafe(offset + byteSize, value(i))
          i += 1
        }
        byteSize
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
      else {
        val array = new Array[Int](length)
        var i = 0
        while(i < length) {
          array(i) = decoder.readInt()
          i += 1
        }
        array
      }
    }
  }

  implicit val arrayLongFormatter: Formatter[Array[Long]] = new Formatter[Array[Long]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[Long]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        encoder.ensureCapacity(offset, 4 + 8 * length)
        var byteSize = encoder.writeIntUnsafe(offset, length)

        var i = 0
        while(i < length) {
          byteSize += encoder.writeLongUnsafe(offset + byteSize, value(i))
          i += 1
        }
        byteSize
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
      else {
        val array = new Array[Long](length)
        var i = 0
        while(i < length) {
          array(i) = decoder.readLong()
          i += 1
        }
        array
      }
    }
  }

  implicit val arrayCharFormatter: Formatter[Array[Char]] = new Formatter[Array[Char]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: Array[Char]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {

        val length = value.length
        encoder.ensureCapacity(offset, 4 + 2 * length)
        var byteSize = encoder.writeIntUnsafe(offset, length)

        var i = 0
        while(i < length) {
          byteSize += encoder.writeCharUnsafe(offset + byteSize, value(i))
          i += 1
        }
        byteSize
      }

    override def deserialize(decoder: Decoder) = {
      val length = decoder.readInt()
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Array length($length).")
      else {
        val array = new Array[Char](length)
        var i = 0
        while(i < length) {
          array(i) = decoder.readChar()
          i += 1
        }
        array
      }
    }
  }

  implicit val listByteFormatter: Formatter[List[Byte]] = {

    @annotation.tailrec
    def go(v: List[Byte], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + encoder.writeByteUnsafe(offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[Byte]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[Byte]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[Byte]()
          var i = 0
          while(i < length) {
            list += decoder.readByte()
            i += 1
          }
          list.toList
        }
      }
    }
  }

  implicit val listBoolFormatter: Formatter[List[Boolean]] = {

    @annotation.tailrec
    def go(v: List[Boolean], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + encoder.writeBoolUnsafe(offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[Boolean]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[Boolean]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[Boolean]()
          var i = 0
          while(i < length) {
            list += decoder.readBool()
            i += 1
          }
          list.toList
        }
      }
    }
  }

  implicit val listShortFormatter: Formatter[List[Short]] = {

    @annotation.tailrec
    def go(v: List[Short], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + encoder.writeShortUnsafe(offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[Short]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[Short]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 2 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[Short]()
          var i = 0
          while(i < length) {
            list += decoder.readShort()
            i += 1
          }
          list.toList
        }
      }
    }
  }

  implicit val listIntFormatter: Formatter[List[Int]] = {

    @annotation.tailrec
    def go(v: List[Int], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + encoder.writeIntUnsafe(offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[Int]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[Int]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 4 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[Int]()
          var i = 0
          while(i < length) {
            list += decoder.readInt()
            i += 1
          }
          list.toList
        }
      }
    }
  }

  implicit val listLongFormatter: Formatter[List[Long]] = {

    @annotation.tailrec
    def go(v: List[Long], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + encoder.writeLongUnsafe(offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[Long]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[Long]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 8 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[Long]()
          var i = 0
          while(i < length) {
            list += decoder.readLong()
            i += 1
          }
          list.toList
        }
      }
    }
  }

  implicit val listCharFormatter: Formatter[List[Char]] = {

    @annotation.tailrec
    def go(v: List[Char], encoder: Encoder, offset: Int, byteSize: Int): Int = {
      if(v.nonEmpty) {
        go(v.tail, encoder, offset, byteSize + encoder.writeCharUnsafe(offset + byteSize, v.head))
      }
      else byteSize
    }

    new Formatter[List[Char]] {
      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: List[Char]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 2 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid List length($length).")
        else {
          val list = scala.collection.mutable.ListBuffer[Char]()
          var i = 0
          while(i < length) {
            list += decoder.readChar()
            i += 1
          }
          list.toList
        }
      }
    }
  }

  implicit val vectorByteFormatter: Formatter[Vector[Byte]] = {

    @annotation.tailrec
    def go(v: Vector[Byte], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + encoder.writeByteUnsafe(offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[Byte]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[Byte]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[Byte]
          var i = 0
          while(i < length) {
            builder += decoder.readByte()
            i += 1
          }
          builder.result()
        }
      }
    }
  }

  implicit val vectorBoolFormatter: Formatter[Vector[Boolean]] = {

    @annotation.tailrec
    def go(v: Vector[Boolean], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + encoder.writeBoolUnsafe(offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[Boolean]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[Boolean]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[Boolean]
          var i = 0
          while(i < length) {
            builder += decoder.readBool()
            i += 1
          }
          builder.result()
        }
      }
    }
  }

  implicit val vectorShortFormatter: Formatter[Vector[Short]] = {

    @annotation.tailrec
    def go(v: Vector[Short], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + encoder.writeShortUnsafe(offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[Short]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[Short]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 2 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[Short]
          var i = 0
          while(i < length) {
            builder += decoder.readShort()
            i += 1
          }
          builder.result()
        }
      }
    }
  }

  implicit val vectorIntFormatter: Formatter[Vector[Int]] = {

    @annotation.tailrec
    def go(v: Vector[Int], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + encoder.writeIntUnsafe(offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[Int]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[Int]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 4 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[Int]
          var i = 0
          while(i < length) {
            builder += decoder.readInt()
            i += 1
          }
          builder.result()
        }
      }
    }
  }

  implicit val vectorLongFormatter: Formatter[Vector[Long]] = {

    @annotation.tailrec
    def go(v: Vector[Long], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + encoder.writeLongUnsafe(offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[Long]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[Long]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 8 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[Long]
          var i = 0
          while(i < length) {
            builder += decoder.readLong()
            i += 1
          }
          builder.result()
        }
      }
    }
  }

  implicit val vectorCharFormatter: Formatter[Vector[Char]] = {

    @annotation.tailrec
    def go(v: Vector[Char], encoder: Encoder, offset: Int, byteSize: Int, i: Int, length: Int): Int = {
      if(i < length) {
        go(v, encoder, offset, byteSize + encoder.writeCharUnsafe(offset + byteSize, v(i)), i + 1, length)
      }
      else byteSize
    }

    new Formatter[Vector[Char]] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: Vector[Char]) =
        if(value == null) encoder.writeInt(offset, -1)
        else {

          val length = value.length
          encoder.ensureCapacity(offset, 4 + 2 * length)
          encoder.writeIntUnsafe(offset, length)

          go(value, encoder, offset, 4, 0, length)
        }

      override def deserialize(decoder: Decoder) = {
        val length = decoder.readInt()
        if(length == -1) null
        else if(length < -1) throw FormatException(decoder.offset - 4, s"Invalid Vector length($length).")
        else {
          val builder = Vector.newBuilder[Char]
          var i = 0
          while(i < length) {
            builder += decoder.readChar()
            i += 1
          }
          builder.result()
        }
      }
    }
  }
}

abstract class FormatterInstances0 extends FormatterInstances1 {

  def nullableFormatter[T](implicit F: Formatter[T]): Formatter[Option[T]] = new Formatter[Option[T]] {
    override val length = F.length.map(_ + 1)

    override val default = None

    override def serialize(encoder: Encoder, offset: Int, value: Option[T]) = value match {
      case Some(v) =>
        val byteSize = F.length.fold(encoder.writeBool(offset, true))(
          l => {
            encoder.ensureCapacity(offset, l)
            encoder.writeBoolUnsafe(offset, true)
          }
        )
        byteSize + F.serialize(encoder, offset + 1, v)
      case None => encoder.writeBool(offset, false)
    }

    override def deserialize(decoder: Decoder) = {
      if(decoder.readBool()) Some(F.deserialize(decoder))
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

  implicit def tuple2OptionFormatter[A1, A2](implicit F: Formatter[(A1, A2)]): Formatter[Option[(A1, A2)]] =
    nullableFormatter[(A1, A2)]
}
