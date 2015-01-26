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
trait Filter {
  def handles[T: ClassTag]: Boolean

  def readField[T: Reader: ClassTag](name: String, value: Option[Js.Value],
                                     default: Option[T]): Either[ErrorFactory, T]

  def writeField[T: Writer: ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value]

  def orElse(other: Filter): Filter = new Filter {
    override def handles[T: ClassTag] = this.handles[T] && other.handles[T]

    override def readField[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                 default: Option[T]): Either[ErrorFactory, T] =
      (if (this.handles[T]) this else other).readField(name, value, default)

    override def writeField[T: Writer : ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] =
      (if (this.handles[T]) this else other).writeField(name, value, default)
  }
}

object Filter {
  type ErrorFactory = (Js.Value) => Exception

  private def read[T: Reader](value: Js.Value) = implicitly[Reader[T]].read(value)
  private def write[T: Writer](value: T) = implicitly[Writer[T]].write(value)

  val defaultValuesFilter: Filter = new Filter {
    override def handles[T: ClassTag]: Boolean = true

    override def readField[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                 default: Option[T]): Either[ErrorFactory, T] = {
      value.map(read[T]).orElse(default)
        .toRight((o: Js.Value) => throw new Invalid.Data(o, s"Key missing: $name"))
    }

    override def writeField[T: Writer : ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] = {
      if (Some(value) == default) None
      else Some(write(value))
    }
  }

  val optionFieldsFilter: Filter = new Filter {
    override def handles[T: ClassTag] = classTag[T] == classTag[Option[_]]

    override def readField[T: Reader : ClassTag](name: String, value: Option[Js.Value],
                                                 default: Option[T]): Either[ErrorFactory, T] = {
      Right(value.map(read[T] /* Some(..) */).getOrElse(None.asInstanceOf[T]))
    }

    override def writeField[T: Writer : ClassTag](name: String, value: T, default: Option[T]): Option[Js.Value] =
      if (value.asInstanceOf[Option[_]].isDefined) Some(write(value))
      else None
  }
}
