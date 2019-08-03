{ name =
    "fusionauth"
, dependencies =
    [ "aff"
    , "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "arrays"
    , "debug"
    , "datetime"
    , "either"
    , "generics-rep"
    , "formatters"
    , "string-parsers"
    , "profunctor-lenses"
    , "lists"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "ordered-collections"
    , "effect"
    , "console"
    , "partial"
    , "psci-support"
    , "stringutils"
    , "unicode"
    , "uri"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
