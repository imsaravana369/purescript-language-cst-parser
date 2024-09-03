let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230401/packages.dhall
        sha256:d385eeee6ca160c32d7389a1f4f4ee6a05aff95e81373cdc50670b436efa1060

in  upstream
  with foreign-generic =
      { dependencies =
        [ "arrays"
        , "assert"
        , "bifunctors"
        , "console"
        , "control"
        , "effect"
        , "either"
        , "exceptions"
        , "foldable-traversable"
        , "foreign"
        , "foreign-object"
        , "identity"
        , "lists"
        , "maybe"
        , "newtype"
        , "partial"
        , "prelude"
        , "typelevel-prelude"
        , "record"
        , "strings"
        , "transformers"
        , "tuples"
        , "unsafe-coerce"
        ]
      , repo = "https://github.com/imsaravana369/purescript-foreign-generic.git"
      , version = "v0.15.0-updates"
      }

