package com.github.ac27182

package object Module2 {
  object Section2 {
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

  object Section3 {
    //generic coproducts
    import shapeless.{Coproduct, :+:, CNil, Inl, Inr, Generic, ::, HNil}

    case class Red()
    case class Amber()
    case class Green()

    type Light = Red :+: Amber :+: Green :+: CNil

    // :+: we can think of as either
    // CNil we can think of x or y or z

    val red: Light =
      Inl(Red())

    val green: Light =
      Inr(Inr(Inl(Green())))

    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    // example of a generic trait
    val gen0: Generic[Shape] {
      type Repr = Circle :+: Rectangle :+: CNil
      // Repr of the generic shape is a coproduct of the subtypes of the sealed trait shape
    } = Generic[Shape]

    // examples of how a generic case class differs from a generic trait
    val gen1: Generic[Rectangle] {
      type Repr = Double :: Double :: HNil
    } = Generic[Rectangle]

    val gen2: Generic[Circle] {
      type Repr = Double :: HNil
    } = Generic[Circle]

    val e0 = gen0.to(Rectangle(10d, 20d))

    val e1 = gen0.to(Circle(1d))

    // new synatx
    // :+:
    // CNil
    // Inr
    // Inl

  }

}
