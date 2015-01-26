package upickle.config

import upickle.{Filter, Annotator}

trait OldConfig extends Config {
  override implicit val annotator = Annotator.arrayAnnotator
  override implicit val filter = Filter.defaultValuesFilter
}

object old extends OldConfig
