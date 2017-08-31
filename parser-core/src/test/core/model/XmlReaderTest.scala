// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import
  cats.data.Validated.{ Invalid, Valid }

import
  org.scalatest.FunSuite

class XmlReaderTest extends FunSuite {
  import DummyXML._

  test("xml choice reader") {
    val choiceA = namedText("a", "xyz")
    val choiceB = namedText("b", "123")
    val wrongChoice = namedText("c", "xyz")

    val aReader = XmlReader.elemReader("a")
    val bReader = XmlReader.elemReader("b")
    val reader = XmlReader.choiceElementReader(Seq(aReader, bReader)).atPath("foo")

    assertResult(namedText("a", "xyz"))(reader.read(choiceA).toOption.get)
    assertResult(namedText("b", "123"))(reader.read(choiceB).toOption.get)
    assertResult(new MissingElement(Seq("foo"), "a or b"))(reader.read(wrongChoice).swap.toOption.get)
  }

  test("xml sequence reader") {
    val seqEmpty = Elem("seq", Seq(), Seq())
    val seqA = Elem("seq", Seq(), Seq(namedText("a", "xyz")))
    val seqAA = Elem("seq", Seq(), Seq(namedText("a", "xyz"), namedText("a", "123")))
    val seqB = Elem("seq", Seq(), Seq(namedText("b", "ABC")))

    val reader = XmlReader.sequenceElementReader("seq", 1, XmlReader.elemReader("a").map(XmlReader.childText _))
    assertResult(Invalid(MissingElement(Seq("seq"), "a")))(reader.read(seqEmpty))
    assertResult(Seq("xyz"))(reader.read(seqA).toOption.get)
    assertResult(Seq("xyz", "123"))(reader.read(seqAA).toOption.get)
    assertResult(Invalid(MissingElement(Seq("seq"), "a")))(reader.read(seqB))
  }

  test("xml chain sequence reader reads empty xml element") {
    val seqEmpty = Elem("seq", Seq(), Seq())

    val reader = XmlReader.chainSequenceElementReader("seq", 0, XmlReader.elemReader("a").map(XmlReader.childText _))

    assertResult(Valid((Seq(), Seq())))(reader.read(seqEmpty))
  }

  test("xml pointsReader") {
    val xml = Elem("x", Seq(Attr("points", "1,2 4,5 7,9")), Seq())
    val missingNumXml = Elem("x", Seq(Attr("points", "1,")), Seq())
    val invalidNumXml = Elem("x", Seq(Attr("points", "1,abc")), Seq())
    val threeNumXml = Elem("x", Seq(Attr("points", "1,2,3")), Seq())
    val reader = XmlReader.pointsReader("points")
    assertResult(Valid(Seq((1, 2), (4, 5), (7, 9))))(reader.read(xml))
    assertResult(Invalid(InvalidAttribute("points", "1,")))(reader.read(missingNumXml))
    assertResult(Invalid(InvalidAttribute("points", "1,abc")))(reader.read(invalidNumXml))
    assertResult(Invalid(InvalidAttribute("points", "1,2,3")))(reader.read(threeNumXml))
  }
}
