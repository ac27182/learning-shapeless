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
}
