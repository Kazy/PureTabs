{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pure-tabs"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "aff-promise"
  , "avar"
  , "console"
  , "css"
  , "datetime"
  , "debug"
  , "effect"
  , "foreign"
  , "foreign-generic"
  , "generics-rep"
  , "halogen"
  , "halogen-css"
  , "halogen-hooks"
  , "lists"
  , "numbers"
  , "ordered-collections"
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
