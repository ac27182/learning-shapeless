package com.github.ac27182
object Main extends App {
  println(">application operational")
}

object M1 {
  object S1 {
    import shapeless.{Generic, ::, HNil}
    // both case classes represent different kinds of data, but they have clear similaries
    case class Employee(name: String, number: Int, manager: Boolean)
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    // both of these values are of the same type, they are now both hetrogeneous lists
    // here we define a type alias representing the underlying type structure of these two related case classes
    type T0 = String :: Int :: Boolean :: HNil

    val genericEmployee: T0 =
      Generic[Employee].to(Employee("alex", 10, false))

    val genericIceCream: T0 =
      Generic[IceCream].to(IceCream("sundae", 1, false))

    def genericCsv(gen: T0): List[String] =
      List(gen(0), gen(1).toString, gen(2).toString)

    val csvEmployee: List[String] =
      genericCsv(gen = genericEmployee)

    val csvIceCream: List[String] =
      genericCsv(gen = genericIceCream)
  }

}

object M2 {
  object S1 {}
  object S2 {
    import shapeless.{HList, ::, HNil, Generic}

    val product: String :: Int :: Boolean :: HNil =
      "sunday" :: 1 :: false :: HNil

    val e0: String =
      product.head

    val e1: Int =
      product.tail.head

    val e2: Boolean :: HNil =
      product.tail.tail

    val newProduct: Long :: String :: Int :: Boolean :: HNil =
      69L :: product

    // switching representations using generic
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)
    case class Employee(name: String, number: Int, manager: Boolean)

    val iceCreamGen: Generic[IceCream] {
      type Repr = String :: Int :: Boolean :: shapeless.HNil
    } =
      Generic[IceCream]

    // has a to and from method to convert to and from a generic type

    val iceCream0: IceCream =
      IceCream("sundae", 1, false)

    val repr: String :: Int :: Boolean :: HNil =
      iceCreamGen.to(iceCream0)

    val iceCream1: IceCream =
      iceCreamGen.from(repr)

    val employee: Employee =
      Generic[Employee].from(Generic[IceCream].to(iceCream0))
  }

  object S3 {
    //generic coproducts
    import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
  }

  object S4 {}
}
