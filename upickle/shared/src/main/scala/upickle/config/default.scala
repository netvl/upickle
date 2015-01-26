package upickle.config

import upickle.{Filter, Annotator}

trait DefaultConfig extends Config {
  override implicit val annotator = Annotator.keyAnnotator("$variant")
  override implicit val filter = Filter.defaultValuesFilter
}

object default extends DefaultConfig
