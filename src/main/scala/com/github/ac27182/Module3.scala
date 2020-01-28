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
            val (a, b) = pair
            aEncoder.encode(a) ++ bEncoder.encode(b)
          }
        }
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

  object Section2 {}
  object Section3 {}
  object Section4 {}

}
