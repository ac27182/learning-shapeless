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

object M4 {}
object M5 {}
object M6 {}
object M7 {}
