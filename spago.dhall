{ name = "soltan"
, packages = ./packages.dhall
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "codec"
  , "codec-argonaut"
  , "console"
  , "coroutines"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "halogen"
  , "halogen-formless"
  , "halogen-subscriptions"
  , "http-methods"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "record"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-socket"
  , "web-storage"
  , "web-uievents"
  ]
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
