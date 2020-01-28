package com.github.ac27182

object Module3 {
  object Section1 {
    // type class recapping

    // turn a value of some arbitrary type A into a a csv file
    // behavior
    trait CsvEncoder[A] {
      def encode(value: A): List[String]
    }

    // some custom data type
    case class Employee(name: String, number: Int, manager: Boolean)

    // csvencoder instance from the custom datatype
    // instances
    implicit val employeeEncoder: CsvEncoder[Employee] =
      new CsvEncoder[Employee] {
        def encode(employee: Employee): List[String] = List(
          employee.name,
          employee.number.toString,
          if (employee.manager) "yes" else "no"
        )
      }

    // callable function to write csv
    def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
      values
        .map(value => encoder.encode(value).mkString(","))
        .mkString("\n")

    // provides method like syntax, very classy
    implicit class CsvEncoderOps[A](values: List[A]) {
      def writeCsv(implicit encoder: CsvEncoder[A]): String =
        values
          .map(value => encoder.encode(value).mkString(","))
          .mkString("\n")

      def pairEncoder[A, B](
          implicit
          aEncoder: CsvEncoder[A],
          bEncoder: CsvEncoder[B]
      ): CsvEncoder[(A, B)] = new CsvEncoder[(A, B)] {
        def encode(pair: (A, B)): List[String] = {
          // destructureing the pair, javascript style...
          val (a, b) = pair
          aEncoder.encode(a) ++ bEncoder.encode(b)
        }
      }
    }

    val e0 = Employee("alex", 7, false)
    val e1 = Employee("stuart", 10, false)
    val e2 = Employee("ann", 13, true)
    val employees: List[Employee] =
      List(e0, e1, e2)

    // here when we call writecsv, the compiler actually calculates the value of the type parameter
    // and searches for an implicit csv encoder of the corresponding type
    val employeesAsCsv0: String =
      writeCsv(employees)

    val employeesAsCsv: String =
      employees.writeCsv
    // we can hency use write csv with any data type we like

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    // casually defining another encoder for the icecreams
    implicit val iceCreamEncoder: CsvEncoder[IceCream] =
      new CsvEncoder[IceCream] {
        def encode(value: IceCream): List[String] =
          List(
            value.name,
            value.numCherries.toString,
            if (value.inCone) "y" else "n"
          )
      }

    import shapeless.{Generic}

    val e3 = IceCream("cherry", 1, false)
    val e4 = IceCream("chcolate", 2, true)
    val iceCreams = List(e3, e4)

    val iceCreamAsCsv0: String =
      writeCsv(iceCreams)
    val iceCreamAsCsv1: String =
      iceCreams.writeCsv

    implicit def pairEncoder[A, B](
        implicit
        aEncoder: CsvEncoder[A],
        bEncoder: CsvEncoder[B]
    ): CsvEncoder[(A, B)] = new CsvEncoder[(A, B)] {
      def encode(pair: (A, B)): List[String] = {
        // destructureing the pair, javascript style...
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

    val employesAndIceCream = employees zip iceCreams
    // functoinal programming meme magic
    val csv = employesAndIceCream.writeCsv

    // making the code more concise
    object CsvEncoder {
      // this is called a "summoner method"
      def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] = encoder
      // this ic called a constructor method
      def instance[A](func: A => List[String]): CsvEncoder[A] =
        new CsvEncoder[A] {
          def encode(value: A): List[String] = func(value)
        }
    }
    // globally gisible typeclass instances

    import shapeless.{the}

    val e5 = the[CsvEncoder[IceCream]]

    // the instance method (sometimes called pure) provides us with a terse syntax for creating new typeclass instances
    // reducing the boileplate of anonymous class syntax

    // the below two implicit values are equivilent
    implicit val booleanEncoder0: CsvEncoder[Boolean] =
      new CsvEncoder[Boolean] {
        def encode(value: Boolean): List[String] =
          if (value) List("yes") else List("no")
      }

    implicit val booleanEncoder1: CsvEncoder[Boolean] =
      CsvEncoder.instance(b => if (b) List("yes") else List("no"))

  }

  object TypeclassExample0 {
    trait CsvEncoder[A] {
      def encode(value: A): List[String]
    }
    object CsvEncoder {
      def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] = encoder
      def instance[A](func: A => List[String]): CsvEncoder[A] =
        new CsvEncoder[A] {
          def encode(value: A): List[String] = func(value)
        }
    }
    case class Employee(name: String, number: Int, manager: Boolean)
    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    object CsvEncoderInstances {
      implicit val employeeEncoder: CsvEncoder[Employee] =
        new CsvEncoder[Employee] {
          def encode(employee: Employee): List[String] = List(
            employee.name,
            employee.number.toString,
            if (employee.manager) "yes" else "no"
          )
        }
      implicit val iceCreamEncoder: CsvEncoder[IceCream] =
        new CsvEncoder[IceCream] {
          def encode(value: IceCream): List[String] =
            List(
              value.name,
              value.numCherries.toString,
              if (value.inCone) "y" else "n"
            )
        }
    }
    object CsvEncoderSyntax {
      implicit class CsvEncoderOps[A](values: List[A]) {
        def writeCsv(implicit encoder: CsvEncoder[A]): String =
          values
            .map(value => encoder.encode(value).mkString(","))
            .mkString("\n")

        def pairEncoder[A, B](
            implicit
            aEncoder: CsvEncoder[A],
            bEncoder: CsvEncoder[B]
        ): CsvEncoder[(A, B)] = new CsvEncoder[(A, B)] {
          def encode(pair: (A, B)): List[String] = {
            // destructureing the pair, javascript style...
            val (a, b) = pair
            aEncoder.encode(a) ++ bEncoder.encode(b)
          }
        }
      }
    }

    // magical typeclass examples
    // thank you dave gurnell
    val employees: List[Employee] =
      List(
        Employee("alex", 7, false),
        Employee("stuart", 10, false),
        Employee("ann", 13, true)
      )
    val icecreams: List[IceCream] =
      List(
        IceCream("cherry", 1, false),
        IceCream("vanilla", 3, false),
        IceCream("chcolate", 2, true)
      )

    import CsvEncoderInstances._
    import CsvEncoderSyntax.CsvEncoderOps

    val csv0 = icecreams.writeCsv
    val csv1 = employees.writeCsv

  }

  // deriving instances for products
  object Section2 {
    import TypeclassExample0._
    import TypeclassExample0.CsvEncoderInstances.employeeEncoder
    import TypeclassExample0.CsvEncoderSyntax._

    def createEncoder[A](f: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        def encode(value: A): List[String] = f(value)
      }
    // creating some instance constructor and csv encoders for string int and boolean
    implicit val stringEncoder: CsvEncoder[String] =
      createEncoder(str => List(str))

    implicit val intEncoder: CsvEncoder[Int] =
      createEncoder(int => List(int.toString))

    implicit val boolenEncoder: CsvEncoder[Boolean] =
      createEncoder(bool => List(if (bool) "yes" else "no"))

    import shapeless.{HList, ::, HNil}

    implicit val hnilEncoder: CsvEncoder[HNil] =
      createEncoder(hnil => Nil)

    implicit def hlistEncoder[H, T <: HList](
        implicit
        hEncoder: CsvEncoder[H],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] =
      createEncoder {
        case h :: t =>
          hEncoder.encode(h) ++ tEncoder.encode(t)
      }
    // with all of this taken together we have some meme magic which allows us to summon
    // csv encoders for any hlist involving strings ints and booleans

    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

    // we have encoded a heterogeneous list into a string
    val e0 = reprEncoder.encode("abc" :: 123 :: true :: HNil)

    // instances for concrete products
    import shapeless.{Generic}

    implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
      val gen = Generic[IceCream]
      val encoder = CsvEncoder[gen.Repr]
      createEncoder(iceCream => encoder.encode(gen.to(iceCream)))
    }

    val e1 = icecreams.writeCsv

    // we cant refer to a type member in one parameter from another parameter in the same block
    // so we have to define a type alias for our representation using a generic parameter R
    implicit def genericEncoder[A, R](
        implicit
        gen: Generic[A] { type Repr = R },
        enc: CsvEncoder[R]
    ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

    // auxillary type aliases
    // the aux pattern is used commonly throughout the shapeless codebase

    // wont compile because there is a type that isnt a case class or a sealed abstract type
    // class Foo(bar: String, baz: Int)
    // val e2 = List(new Foo("abc", 123)).writeCsv

    // wont compile because we havent implemented an encoder for the Date type
    // import java.util.Date
    // case class Booking(room: String, date: Date)
    // val e2 = List(Booking("board room 9", new Date())).writeCsv

  }
  object Section3 {
    import Section2._
    import TypeclassExample0._
    import TypeclassExample0.CsvEncoderSyntax._
    // deriving instances for coproducts
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

    implicit val cnilEncoder: CsvEncoder[CNil] =
      createEncoder(cnil => throw new Exception("nooooo"))

    implicit def coproductEncoder[H, T <: Coproduct](
        implicit
        hEncoder: CsvEncoder[H],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = createEncoder {
      case Inl(h) => hEncoder.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }

    implicit val doubleEncoder: CsvEncoder[Double] =
      createEncoder(d => List(d.toString))

    val shapes: List[Shape] =
      List(
        Rectangle(1d, 2d),
        Circle(10d)
      )
    val e2 = shapes.writeCsv
  }

  object Section4 {
    import TypeclassExample0._
    import TypeclassExample0.CsvEncoderInstances.employeeEncoder
    import TypeclassExample0.CsvEncoderSyntax._
    import Section2._
    import Section3._
    // defining instances for recursive types

    sealed trait Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]

    // implicit divergence
    // implicit resolution

    case class Ding(field1: Int, field2: String)
    case class Dong(field1: Ding)

    // introductiing the lazy type
    // Lazy supresses implicit divergence at compile time, by guarding against over defensive convergence heuristics
    // it defers evalutation of the implicit parameter at runtime, permitting the derivatino of self referential implicits

    import shapeless.{HList, Coproduct, Lazy, ::, :+:, Inl, Inr, Generic}

    implicit def hListEncoder[H, T <: HList](
        implicit
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] = createEncoder {
      case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

    implicit def coproductEncoder[H, T <: Coproduct](
        implicit
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = createEncoder {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }

    implicit def genericEncoder[A, R](
        implicit
        gen: Generic.Aux[A, R],
        rEncoder: Lazy[CsvEncoder[R]]
    ): CsvEncoder[A] = createEncoder { value =>
      rEncoder.value encode (gen to value)
    }

    // the above prevents the compiler giving up premeturely when dealing with recursive instances of types

  }

  object Section5 {
    import TypeclassExample0._
    import TypeclassExample0.CsvEncoderInstances.employeeEncoder
    import TypeclassExample0.CsvEncoderSyntax._
    import Section2._
    import Section3._
    import Section4._
    // debugging implicit resolutions

    // you can debug using Implicitly
    import shapeless.{Generic, ::, HNil}
    case class Foo(bar: Int, baz: Float)
    type T0 = Int :: Float :: HNil

    // if the HList fails, then try each of its constituent types, until we find one which has not been implemented
    // because float fails we have to create a type class instance for the particular type

    implicit val floatEncoder: CsvEncoder[Float] =
      createEncoder(f => List(f.toString))

    // val t0 = CsvEncoder[Float]

    // debugging using reify
    // to reify: to make something "more concrete"
    // you need to explicitly import the runtime reflection package in scala to do this

    import scala.reflect.runtime.universe.{reify}
    val r0 = reify(CsvEncoder[Int])
    println(r0)
  }

}
