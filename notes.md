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

`coproduct or categorical sum:` is a construction which includes the disjoint union of sets and topological spaces, the free product of hroups and the direct sum of modules and vector spaces. the coproduct of a family of object is essentially the least specific object, which each object in the family admits a morphism

- `given a type A and a HList of type R, an implicit Generic to map A to R and a CsvEncoder for R, create a CsvEncoder for A`

- the compiler uses heuristics to determine whether it is converging on a solution. If the heuristics dont yield favourable results for a particular branch of search, the compiler assumes the branch is not converging and moves on to another branch

- chapter 3 sumary...
