package com.github.ac27182

// continuing with types
object Module8 extends App {

  object Section1 {
    import shapeless.{Nat, Succ}

    type Zero = Nat._0
    type One = Succ[Zero]
    type Two = Succ[One]

    import shapeless.ops.nat.ToInt
    val toInt = ToInt[Two]
    val e0 = toInt.apply
  }

  object Section2 {
    import shapeless._
    import shapeless.ops.{hlist, coproduct, nat}

    val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]

    val coproductLength = coproduct.Length[Double :+: Char :+: CNil]

    trait SizeOf[A] {
      def value: Int
    }

    def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

    implicit def genericSizeOf[A, L <: HList, N <: Nat](
        implicit generic: Generic.Aux[A, L],
        size: hlist.Length.Aux[L, N],
        sizeToInt: nat.ToInt[N]
    ): SizeOf[A] = new SizeOf[A] {
      val value = sizeToInt.apply
    }

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    val e0 = sizeOf[IceCream]
  }

  // case study, a random value generator
  object Section3 {
    // import org.scalacheck._

    // for (i <- 1 to 3)
    //   println(Arbitrary.arbitrary[Int].sample match {
    //     case Some(value) => value
    //     case None        => 0
    //   })

    // for (i <- 1 to 3)
    //   println(Arbitrary.arbitrary[(Boolean, Byte)].sample match {
    //     case Some(value) => value
    //     case None        => (false, 0.toByte)
    //   })

    trait Random[A] {
      def get: A
    }
    def random[A](implicit r: Random[A]): A = r.get

    // instance constructor
    def createRandom[A](f: () => A): Random[A] =
      new Random[A] {
        def get: A = f()
      }

    implicit val intRandom: Random[Int] =
      createRandom(() => scala.util.Random.nextInt(10))

    implicit val charRandom: Random[Char] =
      createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)

    implicit val booleanRandom: Random[Boolean] =
      createRandom(() => scala.util.Random.nextBoolean)

    // for (i <- 1 to 10) println(random[Int])
    // for (i <- 1 to 10) println(random[Char])
    // for (i <- 1 to 10) println(random[Boolean])

    import shapeless._

    implicit def genericRandom[A, R](
        implicit gen: Generic.Aux[A, R],
        random: Lazy[Random[R]]
    ): Random[A] = createRandom(() => gen from (random.value.get))

    implicit val hnilRandom: Random[HNil] = createRandom(() => HNil)

    implicit def hlistRandom[H, T <: HList](
        implicit
        hRandom: Lazy[Random[H]],
        tRandom: Random[T]
    ): Random[H :: T] = createRandom(() => hRandom.value.get :: tRandom.get)

    case class Cell(col: Char, row: Int)

    // for (i <- 1 to 5) println(random[Cell])

    implicit val cnilRandom: Random[CNil] =
      createRandom(() => throw new Exception("inconceivable"))

    implicit def coproductRandom[H, T <: Coproduct](
        implicit
        hRandom: Lazy[Random[H]],
        tRandom: Random[T]
    ): Random[H :+: T] = createRandom { () =>
      val chooseH = scala.util.Random.nextDouble < 0.5
      if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
    }

    sealed trait Light
    case object Red extends Light
    case object Amber extends Light
    case object Green extends Light

    import shapeless.ops.coproduct
    import shapeless.ops.nat.ToInt

    implicit def coproductRandom[H, T <: Coproduct, L <: Nat](
        implicit
        hRandom: Lazy[Random[H]],
        tRandom: Random[T],
        tLength: coproduct.Length.Aux[T, L],
        tLengthAsInt: ToInt[L]
    ): Random[H :+: T] = {
      createRandom { () =>
        val length = 1 + tLengthAsInt()
        val chooseH = scala.util.Random.nextDouble < (1.0 / length)
        if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
      }
    }

    // for (i <- 1 to 10) println(random[Light])

  }

  // other operations involving nat
  object Section4 {
    import shapeless._

    val hlist = 123 :: "foo" :: true :: 'x' :: HNil

    val e0 = hlist.apply[Nat._1]
    val e1 = hlist.apply[Nat._3]
    val e2 = hlist.take(Nat._3).drop(Nat._1)
    val e3 = hlist.updatedAt(Nat._1, "bar").updatedAt(Nat._2, "baz")

  }

  // evaluating Section3
  // Section3

}
