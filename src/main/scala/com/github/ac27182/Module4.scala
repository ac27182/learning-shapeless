package com.github.ac27182

package object Module4 {
  object Section1 {
    // dependent types
    // Generic: the type class for mapping ADT => generic representations
    // trait Generic[A] {
    //   type Repr
    //   def to(value: A): Repr
    //   def from(value: Repr): A
    // }

    import shapeless.Generic

    def getRepr[A](value: A)(implicit gen: Generic[A]) = gen.to(value)
    // type member Repr

    case class Vec(x: Int, y: Int)
    case class Rect(origin: Vec, size: Vec)
    val e0 = getRepr(Vec(1, 2))
    val e1 = getRepr(Rect(Vec(0, 0), Vec(10, 10)))

    // this is an example of dependent typing

    trait Generic0[A, Repr]
    def getRepr0[A, R](value: A)(implicit generic: Generic0[A, R]): R =
      ???

    // type parameters are as useful as inputs, and type members are as useful as outputs

  }

  object Section2 {
    import shapeless.ops.hlist.{Last}
    import shapeless.{HList, ::, HNil, the}

    // trait Last[L <: HList] {
    //   type Out
    //   def apply(in: L): Out
    // }

    val e0 =
      Last[String :: Int :: HNil]
    val e1 =
      Last[Int :: String :: HNil]

    val e2 =
      e0("x" :: 1 :: HNil)

    val e3 =
      e1(123 :: "abc" :: HNil)

    trait Second[L <: HList] {
      type Out
      def apply(value: L): Out
    }

    object Second {
      type Aux[L <: HList, O] = Second[L] { type Out = O }
      def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
    }

    val e4 = implicitly[Last[String :: Int :: HNil]]
    val e5 = Last[String :: Int :: HNil]
    val e6 = the[Last[String :: Int :: HNil]]
  }

  object Section3 {
    // chaining dependent functions
    import shapeless.{Generic, HList, HNil, ::}
    import shapeless.ops.hlist.{Last, IsHCons}
    import Section1._

    //Repr <: HList declares that a type variable Repr refers to a subtype of HList
    def lastField[A, Repr <: HList](input: A)(
        implicit
        gen: Generic.Aux[A, Repr],
        last: Last[Repr]
    ): last.Out = last.apply(gen.to(input))

    val e0 = lastField(Rect(Vec(1, 2), Vec(3, 4)))

    def getWrappedValue[A, Repr <: HList, Head, Tail <: HList](input: A)(
        implicit
        gen: Generic.Aux[A, Repr],
        isHCons: IsHCons.Aux[Repr, Head, HNil]
    ): Head = gen.to(input).head
    case class Wrapper(value: Int)

    // find a generic with a suitable Repr for A
    // provide the Repr that has a head type H

    val e1 = getWrappedValue(Wrapper(42))

  }
}
