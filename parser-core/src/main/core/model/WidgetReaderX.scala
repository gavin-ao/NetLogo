// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import org.nlogo.core.{ Output, TextBox, Widget }

// import scala.meta.ClassTag

sealed trait WidgetResult {
  def widget: Option[Widget]
  def isValid: Boolean
  def isInvalid: Boolean = ! isValid
  def errorMessage: Option[String]
}

case class Success(_widget: Widget) extends WidgetResult {
  val widget = Some(_widget)
  def isValid = true
  def errorMessage = None
}

case class Failure(message: String) extends WidgetResult {
  def widget = None
  def isValid = false
  def errorMessage = Some(message)
}

object WidgetReaderX {
  // convert should map from Seq[String] => A instead of String => A eventually
  class AttributeReader[A](name: String, convert: String => A) {
    def read(widgetType: String, attr: Attribute): AttributeResult[A] = {
      try {
        new AttributeSuccess(convert(attr.values.head))
      } catch {
        case e: Exception =>
          new AttributeFailure(s"$widgetType widget has invalid value '${attr.values.head}' for $name attribute")
      }
    }

    def read(widgetType: String, elem: Element): AttributeResult[A] = {
      elem.attributes.find(_.name == name)
        .map(a => read(widgetType, a))
        .getOrElse(new MissingKeys(widgetType, Seq(name)))
    }
  }

  trait AttributeResult[+A] {
    def map[B](f: A => B): AttributeResult[B]
    def flatMap[B](f: A => AttributeResult[B]): AttributeResult[B]
    def toWidgetResult(implicit ev: A <:< Widget): WidgetResult
  }

  class AttributeSuccess[A](value: A) extends AttributeResult[A] {
    def map[B](f: A => B): AttributeResult[B] = new AttributeSuccess(f(value))
    def flatMap[B](f: A => AttributeResult[B]): AttributeResult[B] = f(value)
    def toWidgetResult(implicit ev: A <:< Widget): WidgetResult = Success(value)
  }

  class AttributeFailure(message: String) extends AttributeResult[A] {
    def map[B](f: A => B): AttributeResult[B] = this
    def flatMap[B](f: A => AttributeResult[B]): AttributeResult[B] = this
    def toWidgetResult(implicit ev: Nothing <:< Widget): WidgetResult = Failure(message)
  }

  case class MissingKeys(widgetType: String, keys: Seq[String]) extends AttributeFailure(
    if (keys.length == 1)
      s"$widgetType widget is missing required attribute '${keys.head}'"
    else
      s"$widgetType widget is missing required attributes ${keys.map(k => s"'$k'").mkString(", ")}"
    ) {

      def flatMap[B](f: Nothing => AttributeResult[B]): AttributeResult[B] = {
        nextRes match {
          case MissingKeys(w, otherKeys) => new MissingKeys(widgetType, keys ++ otherKeys)
          case _ => this
        }
      }
    }

  def read(xml: Element): WidgetResult = {
    xml.tag match {
      case "textbox" => Success(TextBox(Some("Wolf Settings"), 186, 130, 299, 148, 11, 0.0, false))
      case "output"  =>
        val left     = new AttributeReader("left",     _.toInt)
        val top      = new AttributeReader("top",      _.toInt)
        val right    = new AttributeReader("right",    _.toInt)
        val bottom   = new AttributeReader("bottom",   _.toInt)
        val fontSize = new AttributeReader("fontSize", _.toInt)

        (for {
          l <- left.read("output", xml)
          t <- top.read("output", xml)
          r <- right.read("output", xml)
          b <- bottom.read("output", xml)
          fnt <- fontSize.read("fontSize", xml)
        } yield Output(l, t, r, b, fnt)).toWidgetResult
        /*
        val missingAttributes =
          requiredAttributes.filter(attrName => ! xml.attributes.exists(_.name == attrName))
        if (missingAttributes.nonEmpty && missingAttributes.length == 1)
          Failure(s"output widget is missing required attribute ${missingAttributes.head}")
        else if (missingAttributes.nonEmpty)
          Failure(s"output widget is missing required attributes ${missingAttributes.mkString(", ")}")
        else
          Success(Output(290, 449, 602, 543, 12))
        */
      case other     => Failure(s"Unknown widget type $other")
    }
  }
}
