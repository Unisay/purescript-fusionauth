{ name =
    "fusionauth"
, dependencies =
    [ "aff"
    , "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "arrays"
    , "datetime"
    , "either"
    , "generics-rep"
    , "formatters"
    , "lists"
    , "maybe"
    , "newtype"
    , "nonempty"
    , "ordered-collections"
    , "effect"
    , "console"
    , "partial"
    , "psci-support"
    , "unicode"
    , "uri"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
