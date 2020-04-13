{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pure-tabs"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "console"
  , "debug"
  , "effect"
  , "foreign"
  , "foreign-generic"
  , "generics-rep"
  , "jquery"
  , "lists"
  , "numbers"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "refs"
  , "st"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
