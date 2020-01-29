package com.github.ac27182

// Shapeless Ops
package object Module6 {

  import scala.annotation.meta.field
  // working with HLists and coproducs

  object Section1 {
    import shapeless._
    val e0 = ("hello" :: true :: HNil).last
    val e1 = ("hello" :: true :: HNil).init

  }
  object Section2 {
    // creating a custom op ("the lemma pattern")

    import shapeless._
    trait Penultimate[L] {
      type Out
      def apply(l: L): Out
    }
    object Penultimate {
      type Aux[L, O] = Penultimate[L] { type Out = O }
      def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p
    }
    import shapeless.ops.hlist

    implicit def hlistPenultimate[L <: HList, M <: HList, O](
        implicit
        init: hlist.Init.Aux[L, M],
        last: hlist.Last.Aux[M, O]
    ): Penultimate.Aux[L, O] = new Penultimate[L] {
      type Out = O
      def apply(l: L): O = last.apply(init.apply(l))
    }

    type BigList = String :: Int :: Boolean :: Double :: HNil

    val bigList: BigList = "foo" :: 123 :: true :: 456d :: HNil

    val e0 = Penultimate[BigList].apply(bigList)

    type TinyList = String :: HNil

    val tinyList = "bar" :: HNil

    // val e1 = Penultimate[TinyList].apply(tinyList)

    implicit class PenultimateOps[A](a: A) {
      def penultimate(implicit inst: Penultimate[A]): inst.Out =
        inst.apply(a)
    }

    val e1 = bigList.penultimate

    // providing a penultlimate for ALL product types by providing an instance based on generic

    implicit def genericPenultimate[A, R, O](
        implicit
        generic: Generic.Aux[A, R],
        penultimate: Penultimate.Aux[R, O]
    ): Penultimate.Aux[A, O] =
      new Penultimate[A] {
        type Out = O
        def apply(a: A): O = penultimate.apply(generic.to(a))
      }

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    val e2 = IceCream("Sundae", 1, false).penultimate

  }

  object Section3 {
    // case class migrations

    case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

    // eg
    // removing fields
    case class IceCreamV2a(name: String, inCone: Boolean)

    // recorder fields
    case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)

    // insert fields
    case class IceCreamV2c(
        name: String,
        inCone: Boolean,
        numCherries: Int,
        numWaffles: Int
    )

    // we want some meme magic as described below
    // val e0 = IceCreamV1("sundae", 100, false).migrateTo[IceCreamV2a]

    trait Migration[A, B] {
      def apply(a: A): B
    }

    implicit class MigrationOps[A](a: A) {
      def migrateTo[B](implicit migration: Migration[A, B]): B =
        migration.apply(a)
    }

    // convert A to its generic representation
    // filter the HList from step 1, only retain fields that are also in B
    // convert the output of step 2 to B

    import shapeless._
    import shapeless.ops.hlist
    import cats.Monoid
    import cats.instances.all._
    import shapeless.labelled._

    implicit def genericMigration[
        A,
        B,
        ARepr <: HList,
        BRepr <: HList,
        Common <: HList,
        Added <: HList,
        Unaligned <: HList
    ](
        implicit
        aGen: LabelledGeneric.Aux[A, ARepr],
        bGen: LabelledGeneric.Aux[B, BRepr],
        inter: hlist.Intersection.Aux[ARepr, BRepr, Common],
        diff: hlist.Diff.Aux[BRepr, Common, Added],
        monoid: Monoid[Added],
        prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
        align: hlist.Align[Unaligned, BRepr]
    ): Migration[A, B] = new Migration[A, B] {
      def apply(a: A): B = bGen.from(
        align(prepend(monoid.empty, inter(aGen to a)))
      )
    }

    // val e2 = IceCreamV1("sundae", 1, true).migrateTo[IceCreamV2c]

    def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] = new Monoid[A] {
      def empty: A = zero
      def combine(x: A, y: A): A = add(x, y)
    }

    implicit val hnilMonoid: Monoid[HNil] =
      createMonoid[HNil](HNil)((x, y) => HNil)

    implicit def emptyHList[K <: Symbol, H, T <: HList](
        implicit
        hMonoid: Lazy[Monoid[H]],
        tMonoid: Monoid[T]
    ): Monoid[FieldType[K, H] :: T] =
      createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) { (x, y) =>
        field[K](hMonoid.value.combine(x.head, y.head)) :: tMonoid.combine(
          x.tail,
          y.tail
        )
      }

    // very powerful
    val e0 = IceCreamV1("sundae", 1, true).migrateTo[IceCreamV2a]
    val e1 = IceCreamV1("sundae", 1, true).migrateTo[IceCreamV2b]
    val e2 = IceCreamV1("sundae", 1, true).migrateTo[IceCreamV2c]

  }

  object Section4 {
    // record ops
    // shapeless record ops provide map like operations of tagged elements, here are some xamples involving good old ice creams

    import shapeless._
    case class IceCream(
        name: String,
        numCherries: Int,
        inCone: Boolean
    )
    val sundae =
      LabelledGeneric[IceCream].to(IceCream("Sundae", 1, false))

    import shapeless.record._

    val e0 = sundae.get('name)
    val e1 = sundae.get('numCherries)
    // val e2 = sundae.get('nomCherries)
    val e2 = sundae.updated('numCherries, 3)
    val e3 = sundae.remove('inCone)

    val e4 = sundae.updateWith('name)("Massive" + _)

    val e5 = sundae.toMap.toList

  }

}
