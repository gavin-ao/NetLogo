// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import java.lang.{ Integer => JInteger }

import org.nlogo.core.{ AgentKind, Button, BoxedValue,
  Chooser, Chooseable, ChooseableBoolean,
  ChooseableDouble, ChooseableList, ChooseableString,
  Color, Horizontal, InputBox, LogoList, Monitor, NumericInput,
  Output, Slider, StringInput, Switch, TextBox,
  UpdateMode, Vertical, View, Widget, WorldDimensions }

import
  cats.{ Applicative, Apply }

import
  cats.kernel.Semigroup

import
  cats.data.Validated,
    Validated.{Invalid, Valid}

import
  scala.util.Try

object WidgetReaderX {
  type ReadValidation = ({ type l[A] = Validated[ParseError, A] })

  sealed trait ParseError {
    def message: String
  }

  trait MissingValues extends ParseError

  case class MissingKeys(widgetType: String, keys: Seq[String]) extends MissingValues {
    val message =
      if (keys.length == 1)
        s"$widgetType widget is missing required attribute '${keys.head}'"
      else
        s"$widgetType widget is missing required attributes ${keys.map(k => s"'$k'").mkString(", ")}"
  }

  case class MissingElement(widgetType: String, elementName: String) extends MissingValues {
    val message = s"expected $widgetType to contain child element $elementName"
  }

  object InvalidValue {
    def apply(widgetType: String, name: String, value: String): InvalidValue =
      new InvalidValue(widgetType, name, value)
  }

  class InvalidValue(val widgetType: String, val name: String, val value: String) extends ParseError {
    override def message = s"$widgetType widget has invalid value '${value}' for $name attribute"
    override def equals(other: Any): Boolean = {
      other match {
        case i: InvalidValue => i.widgetType == widgetType && i.name == name && i.value == value
        case _ => false
      }
    }
  }

  case class RequiredValue(_widgetType: String, _name: String) extends InvalidValue(_widgetType, _name, "") {
    override def message = s"$widgetType widget must have a value for $name"
  }

  case class UnknownWidgetType(widgetType: String) extends ParseError {
    override def message = s"Unknown widget type $widgetType"
  }


  class OptionalAttributeReader[A](widgetType: String, name: String, convert: String => Validated[ParseError, A], orElse: => Validated[ParseError, A]) {
    def read(attr: Attribute): Validated[ParseError, A] = convert(attr.values.head)

    def read(elem: Element): Validated[ParseError, A] =
      elem.attributes
        .find(_.name == name)
        .map(a => read(a))
        .getOrElse(orElse)
  }

  // convert should map from Seq[String] => A instead of String => A eventually
  class AttributeReader[A](widgetType: String, name: String, convert: String => Validated[ParseError, A]) {
    def read(attr: Attribute): Validated[ParseError, A] =
      convert(attr.values.head)

    def read(elem: Element): Validated[ParseError, A] = {
      elem.attributes.find(_.name == name)
        .map(a => read(a))
        .getOrElse(Invalid(MissingKeys(widgetType, Seq(name))))
    }
  }

  class ValidAttributeReader[A](widgetType: String, name: String, convert: String => A)
  extends AttributeReader(widgetType, name, { (s: String) =>
    try {
      Valid(convert(s))
    } catch {
      case e: Exception => Invalid(InvalidValue(widgetType, name, s))
    }
  })

  class ElementReader[A](
    widgetType: String,
    name: String,
    subreader: Element => Validated[ParseError, A],
    orElse: => Validated[ParseError, A]) {

    def this(widgetType: String, name: String, subreader: Element => Validated[ParseError, A]) =
      this(widgetType, name, subreader, Invalid(MissingKeys(widgetType, Seq(name))))

    def read(elem: Element): Validated[ParseError, A] = {
      elem.children.collect {
        case e: Element if e.tag == name => e
      }
        .headOption
        .map(subreader)
        .getOrElse(orElse)
    }
  }

  def textToOption(s: String): Option[String] =
    if (s.isEmpty) None
    else Some(s)

  object OptionalTextReader {
    def read(elem: Element): Validated[ParseError, Option[String]] = {
      val allText =
        elem.children.collect {
          case t: Text => t.text
        }.mkString("")
      Valid(textToOption(allText))
    }
  }

  class RequiredTextReader(widgetType: String, name: String) {
    def read(elem: Element): Validated[ParseError, String] = {
      val allText =
        elem.children.collect {
          case t: Text => t.text
        }.mkString("")
      if (allText.isEmpty) Invalid(RequiredValue(widgetType, name))
      else Valid(allText)
    }
  }

  object AgentKindReader extends AttributeReader("button", "agentKind",
    { (s: String) =>
      Map("observer" -> AgentKind.Observer,
        "turtle"     -> AgentKind.Turtle,
        "patch"      -> AgentKind.Patch,
        "link"       -> AgentKind.Link).get(s)
          .map(Valid(_))
          .getOrElse(Invalid(InvalidValue("button", "agentKind", s)))
    })

  implicit val sg = new Semigroup[ParseError] {
    def combine(x: ParseError, y: ParseError): ParseError = {
      (x, y) match {
        case (x: UnknownWidgetType, _) => x
        case (_, y: UnknownWidgetType) => y
        case (x: MissingKeys, y: MissingKeys) => new MissingKeys(x.widgetType, x.keys ++ y.keys)
        case (x: InvalidValue, y: InvalidValue) => x
        case (x: MissingValues, _) => x
        case (_, y: MissingValues) => y
      }
    }
  }

  def dimensionAttributeReaders(widgetName: String):
    (AttributeReader[Int], AttributeReader[Int], AttributeReader[Int], AttributeReader[Int]) =
    (new ValidAttributeReader(widgetName, "left",   _.toInt),
     new ValidAttributeReader(widgetName, "top",    _.toInt),
     new ValidAttributeReader(widgetName, "right",  _.toInt),
     new ValidAttributeReader(widgetName, "bottom", _.toInt))

  def hexColorToDouble(widgetName: String, keyName: String)(hexString: String): Validated[ParseError, Double] = {
    if (hexString.length < 7)
      Invalid(InvalidValue(widgetName, keyName, hexString))
    else {
      try {
        val (rs, gs, bs) = (hexString.substring(1, 3), hexString.substring(3, 5), hexString.substring(5, 7))
        val r = JInteger.valueOf(rs, 16)
        val g = JInteger.valueOf(gs, 16)
        val b = JInteger.valueOf(bs, 16)
        Valid(Color.getClosestColorNumberByARGB(Color.getRGBInt(r, g, b)))
      } catch {
        case e: NumberFormatException =>
          println(e)
          Invalid(InvalidValue(widgetName, keyName, hexString))
      }
    }
  }

  def read(xml: Element): Validated[ParseError, Widget] = {
    val readers = Map(
      "button"  -> ButtonReader,
      "chooser" -> ChooserReader,
      "input"   -> InputReader,
      "monitor" -> MonitorReader,
      "output"  -> OutputReader,
      "slider"  -> SliderReader,
      "switch"  -> SwitchReader,
      "textbox" -> TextBoxReader,
      "view"    -> ViewReader)
    readers
      .get(xml.tag)
      .map(_.read(xml))
      .getOrElse(Invalid(UnknownWidgetType(xml.tag)))
  }

  trait Reader {
    def read(xml: Element): Validated[ParseError, Widget]
  }

  trait ReaderHelpers extends Reader {
    def widgetName: String

    val left   = new ValidAttributeReader(widgetName, "left",   _.toInt)
    val top    = new ValidAttributeReader(widgetName, "top",    _.toInt)
    val right  = new ValidAttributeReader(widgetName, "right",  _.toInt)
    val bottom = new ValidAttributeReader(widgetName, "bottom", _.toInt)

    def fontSizeReader =
      new ValidAttributeReader(widgetName, "fontSize", _.toInt)

    def intReader(attributeName: String) =
      new ValidAttributeReader(widgetName, attributeName, _.toInt)

    def booleanReader(name: String) =
      new ValidAttributeReader(widgetName, name, _.toBoolean)

    def doubleReader(name: String) =
      new ValidAttributeReader(widgetName, name, _.toDouble)

    def childText(xml: Element): String =
      xml.children.collect {
        case t: Text => t.text
      }.mkString("")
  }

  object ButtonReader extends ReaderHelpers {
    def widgetName = "button"

    def read(xml: Element): Validated[ParseError, Button] = {
      val forever = booleanReader("forever")
      val buttonKind = AgentKindReader
      val actionKey = new OptionalAttributeReader[Option[Char]]("button", "actionKey",
        s => Valid(textToOption(s).map(_.head)), Valid(None))
      val enabledBeforeTicks = booleanReader("enabledBeforeTicks")
      val display = new ElementReader("button", "display", OptionalTextReader.read _, Valid(None))
      val source = new ElementReader("button", "source", OptionalTextReader.read _, Valid(None))
      Apply[ReadValidation#l].map10(
        left.read(xml),
        top.read(xml),
        right.read(xml),
        bottom.read(xml),
        forever.read(xml),
        buttonKind.read(xml),
        actionKey.read(xml),
        enabledBeforeTicks.read(xml),
        display.read(xml),
        source.read(xml)) {
          case (l, t, r, b, fver, kind, key, ebt, disp, src) =>
            Button(src, l, t, r, b, disp, fver, kind, key, ! ebt)
        }
    }
  }

  object InputReader extends ReaderHelpers {
    def widgetName = "input"

    object NumericInputReader {
      def read(xml: Element): Validated[ParseError, NumericInput] = {
        null
      }
    }

    object StringInputReader {
      def read(xml: Element): Validated[ParseError, StringInput] = {
        xml.children.collect {
          case e: Element if e.tag == "stringInput" =>
            booleanReader("multiline").read(e).map { multiline =>
              val text = childText(e)
              StringInput(text, StringInput.StringLabel, multiline)
            }
        }.headOption.getOrElse(Invalid(MissingElement(widgetName, "stringInput")))
      }
    }

    object BoxedValueReader {
      def read(xml: Element): Validated[ParseError, BoxedValue] = {
        StringInputReader.read(xml) orElse NumericInputReader.read(xml)
      }
    }

    def read(xml: Element): Validated[ParseError, InputBox] = {
      val boxedValue = BoxedValueReader.read(xml)

      val variable =
        new ElementReader("input", "variable", OptionalTextReader.read _, Valid(None))

      Apply[ReadValidation#l].map6(
        left.read(xml), top.read(xml), right.read(xml), bottom.read(xml),
        variable.read(xml), boxedValue) {
          case (l, t, r, b, varName, boxed) => InputBox(varName, l, t, r, b, boxed)
        }
    }
  }

  object MonitorReader extends ReaderHelpers {
    def widgetName = "monitor"

    def read(xml: Element): Validated[ParseError, Monitor] = {
      val display =
        new ElementReader("monitor", "display", OptionalTextReader.read _, Valid(None))
      val source =
        new ElementReader("monitor", "source", OptionalTextReader.read _, Valid(None))
      val fontSize = fontSizeReader
      val precision = new ValidAttributeReader("monitor", "precision", _.toInt)
      Apply[ReadValidation#l].map8(
        left.read(xml), top.read(xml), right.read(xml), bottom.read(xml),
        display.read(xml), source.read(xml), fontSize.read(xml), precision.read(xml)) {
          case (l, t, r, b, disp, src, font, prec) =>
            Monitor(src, l, t, r, b, disp, prec, font)
        }
    }
  }

  object OutputReader extends ReaderHelpers {
    def widgetName = "output"

    def read(xml: Element): Validated[ParseError, Output] = {
      val fontSize = fontSizeReader
      Apply[ReadValidation#l].map5(left.read(xml), top.read(xml), right.read(xml), bottom.read(xml),
        fontSize.read(xml)) {
          case (l, t, r, b, fnt) => Output(l, t, r, b, fnt)
        }
    }
  }

  object SliderReader extends ReaderHelpers {
    def widgetName = "slider"

    object DirectionReader extends AttributeReader("slider", "direction",
    { (s: String) =>
      Map("horizontal" -> Horizontal,
        "vertical"     -> Vertical)
          .get(s)
          .map(Valid(_))
          .getOrElse(Invalid(InvalidValue("slider", "direction", s)))
    })

    val variable =
      new ElementReader("slider", "variable", OptionalTextReader.read _, Valid(None))
    val minimum =
      new ElementReader("slider", "minimum", new RequiredTextReader("slider", "minimum").read _,
        Invalid(MissingKeys(widgetName, Seq("minimum"))))
    val maximum =
      new ElementReader("slider", "maximum", new RequiredTextReader("slider", "maximum").read _,
        Invalid(MissingKeys(widgetName, Seq("maximum"))))
    val step =
      new ElementReader("slider", "step", new RequiredTextReader("slider", "step").read _,
        Invalid(MissingKeys(widgetName, Seq("step"))))
    val units =
      new ElementReader("slider", "units", OptionalTextReader.read _, Valid(None))

    val default = new ValidAttributeReader(widgetName, "default", _.toInt)

    def read(xml: Element): Validated[ParseError, Slider] = {
      Apply[ReadValidation#l].map11(
        left.read(xml),
        top.read(xml),
        right.read(xml),
        bottom.read(xml),
        default.read(xml),
        variable.read(xml),
        minimum.read(xml),
        maximum.read(xml),
        step.read(xml),
        units.read(xml),
        DirectionReader.read(xml)) {
          case (l, t, r, b, dfault, varname, min, max, stp, u, dir) =>
            Slider(varname,
              l, t, r, b, varname,
              min, max, dfault, stp, u, dir)
      }
    }
  }

  object SwitchReader extends ReaderHelpers {
    def widgetName = "switch"

    def read(xml: Element): Validated[ParseError, Switch] = {
      val variableReader =
        new ElementReader("switch", "variable", OptionalTextReader.read _, Valid(None))
      val onReader = booleanReader("isOn")
      Apply[ReadValidation#l].map6(
        left.read(xml), top.read(xml), right.read(xml), bottom.read(xml),
        variableReader.read(xml), onReader.read(xml)) {
          case (l, t, r, b, varName, on) => Switch(varName, l, t, r, b, varName, on)
        }
    }
  }

  object TextBoxReader extends ReaderHelpers {
    def widgetName = "textbox"

    val color: AttributeReader[Double] =
      new AttributeReader(widgetName, "color", hexColorToDouble(widgetName, "color") _)

    val fontSize = fontSizeReader

    def read(xml: Element): Validated[ParseError, TextBox] = {
      val transparent = booleanReader("transparent")
      val display =
        new ElementReader("textbox", "display", OptionalTextReader.read _, Valid(None))
      Apply[ReadValidation#l].map8(
        left.read(xml), top.read(xml), right.read(xml), bottom.read(xml),
        display.read(xml), fontSize.read(xml), color.read(xml), transparent.read(xml)) {
          case (l, t, r, b, d, fnt, c, tr) => TextBox(d, l, t, r, b, fnt, c, tr)
        }
    }
  }

  object ViewReader extends ReaderHelpers {
    def widgetName = "view"

    val fontSize = fontSizeReader

    object UpdateModeReader extends AttributeReader("view", "updateMode",
    { (s: String) =>
      Map("continuous" -> UpdateMode.Continuous,
        "tick-based"   -> UpdateMode.TickBased)
          .get(s)
          .map(Valid(_))
          .getOrElse(Invalid(InvalidValue("view", "updateMode", s)))
    })

    def read(xml: Element): Validated[ParseError, View] = {

      val valDimension =
        Apply[ReadValidation#l].map7(
        intReader("minPxcor").read(xml), intReader("maxPxcor").read(xml),
        intReader("minPycor").read(xml), intReader("maxPycor").read(xml),
        doubleReader("patchSize").read(xml),
        booleanReader("wrapInX").read(xml), booleanReader("wrapInY").read(xml)
      ) {
        case (mnx, mxx, mny, mxy, ps, wx, wy) => WorldDimensions(mnx, mxx, mny, mxy, ps, wx, wy)
      }

      val frameRate = doubleReader("frameRate")
      val tickCounterVisible = booleanReader("tickCounterVisible")
      val tickCounterLabel = new ElementReader("view", "tickCounterLabel", OptionalTextReader.read _, Valid(None))

      Apply[ReadValidation#l].map10(
        left.read(xml), top.read(xml), right.read(xml), bottom.read(xml), valDimension,
        fontSize.read(xml), UpdateModeReader.read(xml),
        tickCounterVisible.read(xml), tickCounterLabel.read(xml), frameRate.read(xml)) {
          case (l, t, r, b, dim, fnt, um, ticksVisible, ticksLabel, fr) =>
            View(l, t, r, b, dim, fnt, um, ticksVisible, ticksLabel, fr)
        }
    }
  }

  object ChooserReader extends ReaderHelpers {
    import cats.instances.list._

    def widgetName = "chooser"

    trait ChooseableReader {
      def read(xml: Element): Validated[ParseError, Chooseable]
    }

    val readers = Map(
      "booleanChoice" -> ChooseableBooleanReader,
      "listChoice"    -> ChooseableListReader,
      "numberChoice"  -> ChooseableDoubleReader,
      "stringChoice"  -> ChooseableStringReader
    )

    object ChooseableBooleanReader extends ChooseableReader {
      def read(xml: Element): Validated[ParseError, ChooseableBoolean] = {
        val allText = childText(xml)
        Try(allText.toBoolean).toOption
          .map(b => ChooseableBoolean(Boolean.box(b)))
          .map(Valid.apply)
          .getOrElse(Invalid(InvalidValue("chooser", "booleanChoice", allText)))
      }
    }

    object ChooseableDoubleReader extends ChooseableReader {
      def read(xml: Element): Validated[ParseError, ChooseableDouble] = {
        val allText = childText(xml)
        Try(allText.toDouble).toOption
          .map(d => ChooseableDouble(Double.box(d)))
          .map(Valid.apply)
          .getOrElse(Invalid(InvalidValue("chooser", "numberChoice", allText)))
      }
    }

    object ChooseableStringReader extends ChooseableReader {
      def read(xml: Element): Validated[ParseError, ChooseableString] = {
        val allText = childText(xml)
        Valid(ChooseableString(allText))
      }
    }

    object ChooseableListReader extends ChooseableReader {
      def read(xml: Element): Validated[ParseError, ChooseableList] = {
        readChooseableList(xml).map(chooseables => ChooseableList(LogoList(chooseables.map(_.value): _*)))
      }
    }

    def readChooseableList(xml: Element): Validated[ParseError, List[Chooseable]] = {
      val validatedChooseableElems =
        xml.children.collect {
          case e: Element if readers.contains(e.tag) => readers(e.tag).read(e)
        }.toList
      Applicative[ReadValidation#l].sequence(validatedChooseableElems)
    }

    object ChoiceReader {
      def read(xml: Element): Validated[ParseError, List[Chooseable]] = readChooseableList(xml)
    }

    def read(xml: Element): Validated[ParseError, Chooser] = {
      val defaultChoice = intReader("defaultChoice")

      val variable =
        new ElementReader("chooser", "variable", OptionalTextReader.read _, Valid(None))

      Apply[ReadValidation#l].map7(
        left.read(xml), top.read(xml), right.read(xml), bottom.read(xml),
        defaultChoice.read(xml), variable.read(xml), ChoiceReader.read(xml)) {
          case (l, t, r, b, choice, varName, allChoices) =>
            Chooser(varName, l, t, r, b, varName, allChoices, choice)
        }.ensureOr(chooser =>
          InvalidValue("chooser", "defaultChoice", chooser.currentChoice.toString))(chooser =>
            chooser.currentChoice < chooser.choices.length)
    }
  }
}
