package com.github.ac27182

// functional operations on HLists

package object Module7 {
  // shapeless poly

  object Section2 {

    // NB, the below code is not actually shapless, just a simplified version of the api for illustrative purposes
    // trait Case[P, A] {
    //   type Result
    //   def apply(a: A): Result
    // }

    // trait Poly {
    //   def apply[A](arg: A)(implicit cse: Case[this.type, A]): cse.Result =
    //     cse.apply(arg)
    // }

    // object MyPoly extends Poly {
    //   implicit def intCase = new Case[this.type, Int] {
    //     type Result = Double
    //     def apply(a: Int): Result = a / 2.0
    //   }
    //   implicit def stringCase = new Case[this.type, String] {
    //     type Result = Int
    //     def apply(a: String): Int = a.length
    //   }
    // }

    // val e0: Double =
    //   MyPoly.apply(123)

    // real poly code
    import shapeless._

    object myPoly extends Poly1 {

      implicit def intCase: Case.Aux[Int, Double] =
        at(num => num / 2.0)

      implicit def stringCase: Case.Aux[String, Int] =
        at(str => str.length)

    }

    val e0 =
      myPoly.apply(123)

    val e1 =
      myPoly.apply("123")

    object multiply extends Poly2 {

      implicit val intIntCase: Case.Aux[Int, Int, Int] =
        at((a, b) => a * b)
      implicit val intStrCase: Case.Aux[Int, String, String] =
        at((a, b) => b * a)

    }

    val e2 =
      multiply(7, 13)

    val e3 =
      multiply(3, "4")

    import scala.math.Numeric
    object total extends Poly1 {

      implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] =
        at(num.toDouble)

      implicit def option[A](
          implicit num: Numeric[A]
      ): Case.Aux[Option[A], Double] =
        at(opt => opt.map(num.toDouble).getOrElse(0.0))

      implicit def list[A](
          implicit num: Numeric[A]
      ): Case.Aux[List[A], Double] = at(list => num.toDouble(list.sum))

    }

    val e4 =
      total(10)

    val e5 =
      total(Option(20.0))

    val e6 =
      total(List(1L, 2L, 3L))

    // NB poly can confuse the scala compiler
    // we can bypass this by adding a type annotation to our line of code
    val e7: Double = myPoly[Int](123)

  }

  // maping and flatmapping using poly
  object Section3 {
    import shapeless._

    object sizeOf extends Poly1 {
      implicit val intCase: Case.Aux[Int, Int] = at(identity)
      implicit val stringCase: Case.Aux[String, Int] = at(_.length)
      implicit val booleanCase: Case.Aux[Boolean, Int] =
        at(bool => if (bool) 1 else 0)
    }

    // nb we needed maching cases inside our size of in poly1
    val e0 = (10 :: "hello" :: true :: HNil)
      .map(sizeOf)

    object valueAndSizeOf extends Poly1 {
      implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
        at(num => num :: num :: HNil)

      implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
        at(str => str :: str.length :: HNil)

      implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
        at(bool => bool :: (if (bool) 1 else 0) :: HNil)

    }

    val e1 =
      (10 :: "Hello" :: true :: HNil).flatMap(valueAndSizeOf)

  }

  // folding using Poly
  object Section4 {
    import shapeless._

    object sum extends Poly2 {
      implicit val intIntCase: Case.Aux[Int, Int, Int] = at((a, b) => a + b)

      implicit val intStringCase: Case.Aux[Int, String, Int] =
        at((a, b) => a + b.length)
    }

    val e0 = (10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum)
  }

  // defining type classes using poly
  object Section5 {
    trait ProductMapper[A, B, P] {
      def apply(a: A): B
    }

    import shapeless._
    import shapeless.ops.hlist

    implicit def genericProductMapper[
        A,
        B,
        P <: Poly,
        ARepr <: HList,
        BRepr <: HList
    ](
        implicit aGen: Generic.Aux[A, ARepr],
        bGen: Generic.Aux[B, BRepr],
        mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
    ): ProductMapper[A, B, P] = new ProductMapper[A, B, P] {
      def apply(a: A): B = bGen from (mapper.apply(aGen to a))
    }

    implicit class ProductMapperOps[A](a: A) {
      class Builder[B] {
        def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B =
          pm apply a
      }

      def mapTo[B]: Builder[B] = new Builder[B]
    }

    object conversions extends Poly1 {
      implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
      implicit val boolCase: Case.Aux[Boolean, Int] = at(if (_) 1 else 0)
      implicit val strCase: Case.Aux[String, String] = at(identity)
    }
    case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
    case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

    val e0 = IceCream1(name = "sundae", numCherries = 1, inCone = false)
      .mapTo[IceCream2](conversions)

  }

}
