package upickle

import scala.annotation.implicitNotFound
import scala.reflect.{ClassTag, classTag}
import Filter.ErrorFactory

/**
 * Represents a transformation applied to fields of case classes when they are pickled or unpickled.
 *
 * This transformation is used to provide default values or determine whether some fields should appear in
 * the output or not.
 *
 * Filters can be combined with `orElse` method.
 */
@implicitNotFound("Couldn't find any Filter implementation in scope. Make sure you have imported some configuration, e.g. `import upickle.config.default._`")
trait Filter { self =>
  def handles[T: ClassTag]: Boolean
  
  private def checkHandles[T: ClassTag](): Unit = 
    if (!this.handles[T]) 
      throw new IllegalStateException(s"Configuration error: current filter can't handle ${implicitly[ClassTag[T]]}")

  def readField[T: Reader: ClassTag](name: String, value: Option[Js.Value],
                                     default: Option[T]): Either[ErrorFactory, T] = {
    checkHandles[T]()
    readField0(name, value, default)
  }

  def writeField[T: Writer: ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] = {
    checkHandles[T]()
    writeField0(name, value, default)
  }

  protected def readField0[T: Reader: ClassTag](name: String, value: Option[Js.Value],
                                     default: Option[T]): Either[ErrorFactory, T]

  protected def writeField0[T: Writer: ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value]

  def orElse(other: Filter): Filter = new Filter {
    override def handles[T: ClassTag] = self.handles[T] || other.handles[T]

    override protected def readField0[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                            default: Option[T]): Either[ErrorFactory, T] =
      (if (self.handles[T]) self else other).readField0(name, value, default)

    override protected def writeField0[T: Writer : ClassTag](name: String, value: T,
                                                             default: Option[T]): Option[Js.Value] =
      (if (self.handles[T]) self else other).writeField0(name, value, default)
  }
}

object Filter {
  type ErrorFactory = (Js.Value) => Exception

  private def read[T: Reader](value: Js.Value) = implicitly[Reader[T]].read(value)
  private def write[T: Writer](value: T) = implicitly[Writer[T]].write(value)
  
  private def keyMissing(key: String)(o: Js.Value) = throw new Invalid.Data(o, s"Key missing: $key")

  val defaultValuesFilter: Filter = new Filter {
    override def handles[T: ClassTag]: Boolean = true

    override def readField0[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                 default: Option[T]): Either[ErrorFactory, T] = {
      value.map(read[T]).orElse(default).toRight(keyMissing(name))
    }

    override def writeField0[T: Writer : ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] = {
      if (Some(value) == default) None
      else Some(write(value))
    }
  }
  
  val identityFilter: Filter = new Filter {
    override def handles[T: ClassTag]: Boolean = true

    override def readField0[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                 default: Option[T]): Either[ErrorFactory, T] = {
      value.map(read[T]).toRight(keyMissing(name))
    }

    override def writeField0[T: Writer : ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] = {
      Some(write(value))
    }
  }

  // This filter only works with the default option serializer defined in upickle.Implicits
  val optionFieldsFilter: Filter = new Filter {
    override def handles[T: ClassTag] = classTag[T] == classTag[Option[_]]

    override def readField0[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                 default: Option[T]): Either[ErrorFactory, T] = {
      Right(value.map(v => read[T](Js.Arr(v)) /* Some(..) */).getOrElse(None.asInstanceOf[T]))
    }

    override def writeField0[T: Writer : ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] =
      if (value.asInstanceOf[Option[_]].isDefined) Some(write(value).asInstanceOf[Js.Arr].value.head)
      else None
  }
}
