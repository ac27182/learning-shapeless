# The type astronauts guide to shapeless

- types are helpful because the are specific: they show us how pieces of code fit together and prevent bugs, and guide us toward solutions when we code

- sometimes types are too specific, and we want to `exploit similarities` between types to avoid repetition

## book outline

- type class derivation
- generic representations
- generic typeclasses
- generic encoding for case classes and sealed traits
- generic derivation of custom typeclasses
- lazy types
- programming patterns to make code generic
- dependent types
- dependently typed functions
- typelevel programming
- labelled generic
- litteraltypes
- phantom types
- type tagging
- ops type classes
- case class migration
- polymorphic functinos
- natural number type
- developing scalachecks arbitrary

- the beauty of adt's is that they're completely typesafe

- they can help us write complete, correctly typed methods involving our types

`heterogeneous list:` a hlist is either the empty list HNil, or a pair ::[H, T] such that H is some arbitrary type and T is a HList

- there is an advantage to keeping our representation types separate from the semantic types used in our applicatoin. The HList provides us with this representation
