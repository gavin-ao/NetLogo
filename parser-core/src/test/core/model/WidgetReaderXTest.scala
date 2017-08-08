// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import org.nlogo.core.{ Output, TextBox }

import org.scalatest.FunSuite

class WidgetReaderXTest extends FunSuite {
  case class Elem(tag: String, attributes: Seq[Attribute], children: Seq[Node]) extends Element
  case class Txt(text: String) extends Text
  object Attr {
    def apply(name: String, value: String) = new Attr(name, Seq(value))
  }
  case class Attr(name: String, values: Seq[String]) extends Attribute

  test("reads TextBox widgets from xml") {
    val xml = Elem("textbox",
      Seq(Attr("left", "186"),
        Attr("top", "130"),
        Attr("right", "299"),
        Attr("bottom", "148"),
        Attr("color", "#FFFFFF"),
        Attr("fontSize", "11"),
        Attr("transparent", "false")),
      Seq(Elem("display", Seq(), Seq(
        Txt("Wolf Settings")))))

    assertResult(TextBox(Some("Wolf Settings"), 186, 130, 299, 148, 11, 0.0, false))(WidgetReaderX.read(xml).widget.get)
  }

  test("reads output widgets from xml") {
    val xml = Elem("output",
      Seq(Attr("left", "290"),
        Attr("top", "449"),
        Attr("right", "602"),
        Attr("bottom", "543"),
        Attr("fontSize", "12")),
      Seq())
    assertResult(Output(290, 449, 602, 543, 12))(WidgetReaderX.read(xml).widget.get)
  }

  test("returns an invalid widget parse when a widget is missing a required field") {
    val xml = Elem("output",
      Seq(Attr("top", "449"),
        Attr("right", "602"),
        Attr("bottom", "543"),
        Attr("fontSize", "12")),
      Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult("output widget is missing required attribute 'left'")(WidgetReaderX.read(xml).errorMessage.get)
  }

  test("returns an invalid widget parse when a widget field has the wrong type") {
    val xml = Elem("output",
      Seq(Attr("left", "abc"),
        Attr("top", "449"),
        Attr("right", "602"),
        Attr("bottom", "543"),
        Attr("fontSize", "12")),
      Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult("output widget has invalid value 'abc' for left attribute")(WidgetReaderX.read(xml).errorMessage.get)
  }

  test("returns an invalid widget parse when a widget is missing multiple required fields") {
    val xml = Elem("output",
      Seq(Attr("right", "602"),
        Attr("bottom", "543")),
      Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult("output widget is missing required attributes left, top, fontSize")(WidgetReaderX.read(xml).errorMessage.get)
  }

  test("returns an invalid widget parse when the widget type is unknown") {
    val xml = Elem("thingamajig", Seq(), Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult("Unknown widget type thingamajig")(WidgetReaderX.read(xml).errorMessage.get)
  }
}
