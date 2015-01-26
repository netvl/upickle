package upickle.config

import upickle.{Filter, Annotator}

/**
 * Represents an object which contains customizations for uPickle behavior.
 *
 * Currently it contains an annotation strategy and a case class fields filter.
 */
trait Config {
  implicit def annotator: Annotator
  implicit def filter: Filter
}
