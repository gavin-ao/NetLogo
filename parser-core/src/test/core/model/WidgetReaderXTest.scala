// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import org.nlogo.core.{ AgentKind, Button, Chooser,
  ChooseableBoolean, ChooseableDouble, ChooseableList, ChooseableString,
  Horizontal, InputBox, LogoList, Monitor, Output, Slider, StringInput, Switch,
  TextBox, UpdateMode, View,
  WorldDimensions, Widget }

import org.scalatest.FunSuite

import WidgetReaderX._

class WidgetReaderXTest extends FunSuite {
  case class Elem(tag: String, attributes: Seq[Attribute], children: Seq[Node]) extends Element
  case class Txt(text: String) extends Text
  object Attr {
    def apply(name: String, value: String) = new Attr(name, Seq(value))
  }
  case class Attr(name: String, values: Seq[String]) extends Attribute

  val dimensions =
    Seq(Attr("left", "150"),
      Attr("top", "200"),
      Attr("right", "250"),
      Attr("bottom", "300"))

  val fontSize = Attr("fontSize", "12")
  val color = Attr("color", "#000000")

  def readToWidget(xml: Element): Widget =
    WidgetReaderX.read(xml).toOption.get

  def readToError(xml: Element): ParseError =
    WidgetReaderX.read(xml).swap.toOption.get

  def namedText(elemTag: String, text: String): Elem =
    Elem(elemTag, Seq(), Seq(Txt(text)))

  test("reads TextBox widgets from xml") {
    val xml = Elem("textbox",
      dimensions ++ Seq(color, fontSize,
        Attr("transparent", "false")),
      Seq(namedText("display", "Wolf Settings")))

    assertResult(TextBox(Some("Wolf Settings"), 150, 200, 250, 300, 12, 0.0, false))(readToWidget(xml))
  }

  test("color reader correctly identifies colors") {
    val xml = Elem("textbox",
      dimensions ++ Seq(fontSize,
        Attr("color", "#FFFFFF"),
        Attr("transparent", "false")),
      Seq(namedText("display", "Wolf Settings")))
    assertResult(TextBox(Some("Wolf Settings"), 150, 200, 250, 300, 12, 9.9, false))(readToWidget(xml))
  }

  test("transparent reader accepts only true/false") {
    val xml = Elem("textbox",
      dimensions ++ Seq(fontSize, color, Attr("transparent", "abc")),
      Seq(namedText("display", "Wolf Settings")))
    assertResult(InvalidValue("textbox", "transparent", "abc"))(readToError(xml))
  }

  {
    val attrs = dimensions ++ Seq(fontSize, color, Attr("transparent", "false"))
    val emptyCases =
      Seq(
        ("textbox node contains no children",
          Elem("textbox", attrs, Seq())),
        ("textbox contains children, but no display element",
          Elem("textbox", attrs, Seq(Elem("whatever", Seq(), Seq(Txt("Turtles")))))),
        ("textbox contains display, but display has no text",
          Elem("textbox", attrs, Seq(Elem("display", Seq(), Seq())))))

    emptyCases.foreach {
      case (name, xml) =>
        test(s"$name leads to display: None") {
          val emptyTextTextBox = TextBox(None, 150, 200, 250, 300, 12, 0.0, false)
          assertResult(emptyTextTextBox)(readToWidget(xml))
        }
    }
  }

  test("reads switch widgets from xml") {
    val xml = Elem("switch",
      dimensions :+ Attr("isOn", "false"),
      Seq(namedText("variable", "foo")))
    assertResult(Switch(Some("foo"), 150, 200, 250, 300, Some("foo"), false))(readToWidget(xml))
  }

  test("reads switch widgets with empty variable name from xml") {
    val xml = Elem("switch",
      dimensions :+ Attr("isOn", "false"),
      Seq(namedText("variable", "")))
    assertResult(Switch(None, 150, 200, 250, 300, None, false))(readToWidget(xml))
  }

  test("reads monitor widgets from xml") {
    val xml = Elem("monitor",
      dimensions ++ Seq(fontSize, Attr("precision", "3")),
      Seq(namedText("source", "5 + 10"),
        namedText("display", "this is the monitor")))
    assertResult(
      Monitor(Some("5 + 10"),
        150, 200, 250, 300,
        Some("this is the monitor"), 3, 12))(readToWidget(xml))
  }

  test("reads button widgets from xml") {
    val xml = Elem("button",
      dimensions ++ Seq(
        Attr("forever", "false"),
        Attr("agentKind", "observer"),
        Attr("actionKey", "c"),
        Attr("enabledBeforeTicks", "true")),
      Seq(namedText("source", "go 100"),
        namedText("display", "go")))
    assertResult(
      Button(
        Some("go 100"), 150, 200, 250, 300,
        Some("go"), false, AgentKind.Observer,
        Some('c'), false))(readToWidget(xml))
  }

  test("reads button widgets without an actionKey from xml") {
    val xml = Elem("button",
      dimensions ++ Seq(
        Attr("forever", "false"),
        Attr("agentKind", "turtle"),
        Attr("enabledBeforeTicks", "false")),
      Seq())
    assertResult(Button(None, 150, 200, 250, 300, None, false, AgentKind.Turtle, None, true))(
      readToWidget(xml))
  }

  test("reads slider widgets") {
    val xml = Elem("slider",
      dimensions ++ Seq(
        Attr("direction", "horizontal"),
        Attr("default", "5")),
      Seq(
        namedText("variable", "foo"),
        namedText("minimum", "0"),
        namedText("maximum", "100"),
        namedText("step", "maximum - minimum / 10"),
        namedText("units", "Foozles")))
    assertResult(Slider(Some("foo"), 150, 200, 250, 300, Some("foo"), "0", "100", 5, "maximum - minimum / 10", Some("Foozles"), Horizontal))(
      readToWidget(xml))
  }

  test("reads view widgets") {
    val xml = Elem("view",
      dimensions ++ Seq(
        Attr("minPxcor", "-25"),
        Attr("maxPxcor", "25"),
        Attr("wrapInX", "true"),
        Attr("minPycor", "-25"),
        Attr("maxPycor", "25"),
        Attr("wrapInY", "true"),
        Attr("patchSize", "13"),
        Attr("updateMode", "continuous"),
        Attr("fontSize", "12"),
        Attr("frameRate", "30.0"),
        Attr("tickCounterVisible", "true")),
      Seq(namedText("tickCounterLabel", "ticks")))

    val dims =
      WorldDimensions(-25, 25, -25, 25, 13.0, true, true)
    assertResult(View(150, 200, 250, 300, dims, 12, UpdateMode.Continuous, true, Some("ticks"), 30.0))(
      readToWidget(xml))
  }

  test("reads chooser widgets") {
    val xml = Elem("chooser",
      dimensions :+ Attr("defaultChoice", "0"),
      Seq(
        namedText("variable", "foo"),
        namedText("numberChoice", "0.0"),
        namedText("stringChoice", "abc"),
        namedText("booleanChoice", "true"),
        Elem("listChoice", Seq(),
          Seq(namedText("booleanChoice", "true"), namedText("stringChoice", "def")))))

    assertResult(Chooser(Some("foo"), 150, 200, 250, 300, Some("foo"),
      List(
        ChooseableDouble(Double.box(0.0)),
        ChooseableString("abc"),
        ChooseableBoolean(Boolean.box(true)),
        ChooseableList(LogoList(Boolean.box(true), "def"))),
      0))(readToWidget(xml))
  }

  test("chooser widgets with an unavailable default choice return invalid") {
    val xml = Elem("chooser",
      dimensions :+ Attr("defaultChoice", "0"),
      Seq())
    assertResult(InvalidValue("chooser", "defaultChoice", "0"))(readToError(xml))
  }

  test("chooser widgets with invalid choices are invalid") {
    val xml = Elem("chooser",
      dimensions :+ Attr("defaultChoice", "0"),
      Seq(namedText("booleanChoice", "abc")))
    assertResult(InvalidValue("chooser", "booleanChoice", "abc"))(readToError(xml))
  }

  test("reads string inputbox widgets") {
    val xml = Elem("input",
      dimensions,
      Seq(
        namedText("variable", "foo"),
        namedText("stringInput", "abc").copy(attributes = Seq(Attr("multiline", "true")))))

    assertResult(
      InputBox(Some("foo"), 150, 200, 250, 300,
        StringInput("abc", StringInput.StringLabel, true)))(
        readToWidget(xml))
  }

  test("reads plot widgets") {
    pending
  }

  test("reads output widgets from xml") {
    val xml = Elem("output", dimensions :+ fontSize, Seq())
    assertResult(Output(150, 200, 250, 300, 12))(readToWidget(xml))
  }

  test("returns an invalid widget parse when a widget is missing required child element") {
    pending
  }

  test("returns an invalid widget parse when a widget is missing a required field") {
    val xml = Elem("output",
      dimensions.tail :+ fontSize,
      Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult(MissingKeys("output", Seq("left")))(readToError(xml))
  }

  test("returns an invalid widget parse when a widget field has the wrong type") {
    val xml = Elem("output",
      dimensions.tail ++ Seq(Attr("left", "abc"), fontSize),
      Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult(InvalidValue("output", "left", "abc"))(readToError(xml))
  }

  test("returns an invalid widget parse when a widget is missing multiple required fields") {
    val xml = Elem("output",
      dimensions.tail.tail,
      Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult(MissingKeys("output", Seq("left", "top", "fontSize")))(readToError(xml))
  }

  test("returns an invalid widget parse when the widget type is unknown") {
    val xml = Elem("thingamajig", Seq(), Seq())
    assert(WidgetReaderX.read(xml).isInvalid)
    assertResult(UnknownWidgetType("thingamajig"))(readToError(xml))
  }
}
