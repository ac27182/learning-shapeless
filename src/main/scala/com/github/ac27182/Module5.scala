package com.github.ac27182

// Accessing names during implicit derivation
package object Module5 {

  import shapeless.Lazy
  object Section1 {
    // litteral types
    object Foo
    val e = Foo

    import shapeless.syntax.singleton._

    var x = 42.narrow
    // x = 69 throws a compiler error because narrow converted 42 into a singleton types

  }
  object Section2 {
    // type tagging and phantom types
    val n: Int =
      69

    trait Cherries

    val numCherries =
      n.asInstanceOf[Int with Cherries]

    // as instance of is lethal, try and avoid it at all costs

    import shapeless.labelled.{KeyTag, field}
    import shapeless.syntax.singleton._

    val n0 = 123
    val n1 = "numCherries" ->> n0

    val e0 = field[Cherries](123)

    type FieldType[K, V] = V with KeyTag[K, V]

    // tags exist pureley at compile time and have no runtime representation

    import shapeless.Witness

    val e1 = "numCherries" ->> 123

    def getFieldName[K, V](value: FieldType[K, V])(
        implicit witness: Witness.Aux[K]
    ): K = witness.value

    val e2 = getFieldName(e1)

    def getFieldValue[K, V](value: FieldType[K, V]): V = value

    val e3 = getFieldValue(e1)

    // records and labelledgenerics
    import shapeless.{HList, ::, HNil}
    val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil
  }

  object Section3 {
    // Deriving product instances with LabelledGeneric

    // good typeclass example hint hint

    sealed trait JsonValue
    case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
    case class JsonArray(items: List[JsonValue]) extends JsonValue
    case class JsonString(items: String) extends JsonValue
    case class JsonNumber(value: Double) extends JsonValue
    case class JsonBoolean(value: Boolean) extends JsonValue
    case object JsonNull extends JsonValue

    // case class for encoding values as json
    trait JsonEncoder[A] {
      def encode(value: A): JsonValue
    }
    object JsonEncoder {
      def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] = encoder
    }

    // define some primative instances
    def createEncoder[A](f: A => JsonValue): JsonEncoder[A] =
      new JsonEncoder[A] {
        def encode(value: A): JsonValue = f(value)
      }

    implicit val stringEncoder: JsonEncoder[String] =
      createEncoder(str => JsonString(str))

    implicit val doubleEncoder: JsonEncoder[Double] =
      createEncoder(num => JsonNumber(num))

    implicit val intEncoder: JsonEncoder[Int] =
      createEncoder(num => JsonNumber(num))

    implicit val booleanEncoder: JsonEncoder[Boolean] =
      createEncoder(bool => JsonBoolean(bool))

    implicit def listEncoder[A](
        implicit encoder: JsonEncoder[A]
    ): JsonEncoder[List[A]] =
      createEncoder(list => JsonArray(list.map(encoder.encode)))

    implicit def optionEncoder[A](
        implicit encoder: JsonEncoder[A]
    ): JsonEncoder[Option[A]] =
      createEncoder(option => option.map(encoder.encode).getOrElse(JsonNull))

    case class IceCream(
        name: String,
        numCherries: Int,
        inCone: Boolean
    )

    val iceCream =
      IceCream("Sundae", 1, false)

    val iceCreamJson: JsonValue =
      JsonObject(
        List(
          "name" -> JsonString("alex cameron"),
          "numCherries" -> JsonNumber(69),
          "inCone" -> JsonBoolean(false)
        )
      )

    import shapeless.LabelledGeneric

    val gen =
      LabelledGeneric[IceCream].to(iceCream)

    // instances for HLists

    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(value: A): JsonObject
    }

    def createObjectEncoder[A](f: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        def encode(value: A): JsonObject = f(value)
      }

    import shapeless.{HList, ::, HNil, Lazy}

    implicit val hnilEncoder: JsonObjectEncoder[HNil] =
      createObjectEncoder(hnil => JsonObject(Nil))

    import shapeless.Witness
    import shapeless.labelled.FieldType

    implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
        implicit
        witness: Witness.Aux[K],
        hEncoder: Lazy[JsonEncoder[H]],
        tEncoder: JsonObjectEncoder[T]
    ): JsonEncoder[FieldType[K, H] :: T] = {
      val fieldName: String =
        witness.value.name

      createObjectEncoder { hl =>
        val head = hEncoder.value.encode(hl.head)
        val tail = tEncoder.encode(hl.tail)
        JsonObject((fieldName, head) :: tail.fields)
      }
    }

    import shapeless.LabelledGeneric
    import Section2._
    import Section1._
    implicit def genericObjectEncoder[A, H](
        implicit
        generic: LabelledGeneric.Aux[A, H],
        hEncoder: Lazy[JsonObjectEncoder[H]]
    ): JsonEncoder[A] = createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }

    // val e0 = JsonEncoder[IceCream].encode(iceCream)
  }

  object Section4 {
    // deriving coproduct instances with labelledgeneric

    import shapeless.LabelledGeneric
    import Section3._

    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Circle(radius: Double) extends Shape

    val e0 = LabelledGeneric[Shape].to(Circle(1d))

    import shapeless.{Coproduct, :+:, CNil, Inl, Inr, Witness, Lazy}
    import shapeless.labelled.FieldType

    implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
      createObjectEncoder(cnil => throw new Exception("nooo"))

    implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
        implicit
        witness: Witness.Aux[K],
        hEncoder: Lazy[JsonEncoder[H]],
        tEncoder: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
      val typeName = witness.value.name
      createObjectEncoder {
        case Inl(h)    => JsonObject(List(typeName -> hEncoder.value.encode(h)))
        case Inr(tail) => tEncoder.encode(tail)
      }
    }

    val shape: Shape = Circle(1d)
    // val e1 = JsonEncoder[Shape].encode(shape)

  }
}
