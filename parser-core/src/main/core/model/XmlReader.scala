// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import java.lang.{ Integer => JInteger }

import
  org.nlogo.core.Color

import
  cats.Applicative

import
  cats.data.Validated,
    Validated.{Invalid, Valid}

object XmlReader {
  def booleanReader(name: String): XmlReader[Boolean] =
    validReader(name, _.toBoolean)

  def characterReader(name: String): XmlReader[Option[Char]] =
    new OptionalAttributeReader(name).map(_.flatMap(textToOption).flatMap(_.headOption))

  def colorReader(name: String): XmlReader[Double] =
    new AttributeReader(name).flatMap(hexColorToDouble(name))

  def doubleReader(name: String): XmlReader[Double] =
    validReader(name, _.toDouble)

  def enumReader[A](options: Map[String, A])(name: String): XmlReader[A] =
    validReader(name, options.apply)

  def intReader(name: String): XmlReader[Int] =
    validReader(name, _.toInt)

  def stringReader(name: String): XmlReader[String] =
    validReader(name, identity)

  def optionalElementReader(name: String): XmlReader[Option[Element]] =
    new OptionalElementReader(name)

  def allElementReader(name: String): XmlReader[Element] =
    new ChildElementReader(name)

  def choiceElementReader[A](readers: Seq[XmlReader[_ <: A]]): XmlReader[A] =
    new ChoiceElementReader[A](readers)

  def elemReader(tag: String): XmlReader[Element] =
    new ElementReader(tag)

  // NOTE: We only support reading homogenous sequences
  def sequenceElementReader[A](widgetType: String, min: Int, reader: XmlReader[A]): XmlReader[List[A]] =
    new SequenceElementReader(widgetType, min, reader)

  def validReader[A](name: String, f: String => A): XmlReader[A] =
    new AttributeReader(name).flatMap { s =>
      try { Valid(f(s)) } catch { case e: Exception => Invalid(InvalidAttribute(Seq(), name, s)) }
    }

  def childText(xml: Element): String =
    xml.children.collect {
      case t: Text => t.text
    }.mkString("")

  def textToOption(s: String): Option[String] =
    if (s.isEmpty) None
    else Some(s)

  def doubleToHexColor(d: Double): String = {
    val i = Color.getARGBbyPremodulatedColorNumber(d) & 0xffffff // strip off alpha channel

    val baseHexString = Integer.toHexString(i)
    val leadingZeros = 6 - baseHexString.length
    s"#${"0" * leadingZeros}${baseHexString}".toUpperCase
  }

  private def hexColorToDouble(keyName: String)(hexString: String): Validated[ParseError, Double] = {
    if (hexString.length < 7)
      Invalid(InvalidAttribute(Seq(), keyName, hexString))
    else {
      try {
        val (rs, gs, bs) = (hexString.substring(1, 3), hexString.substring(3, 5), hexString.substring(5, 7))
        val r = JInteger.valueOf(rs, 16)
        val g = JInteger.valueOf(gs, 16)
        val b = JInteger.valueOf(bs, 16)
        Valid(Color.getClosestColorNumberByARGB(Color.getRGBInt(r, g, b)))
      } catch {
        case e: NumberFormatException =>
          Invalid(InvalidAttribute(Seq(), keyName, hexString))
      }
    }
  }

  class AttributeReader[A](val name: String) extends XmlReader[String] {
    def read(elem: Element): Validated[ParseError, String] =
      elem.attributes.find(_.name == name)
        .map(a => Valid(a.values.head))
        .getOrElse(Invalid(MissingKeys(Seq(), Seq(name))))
  }

  class ChildElementReader(val name: String) extends XmlReader[Element] {
    import cats.syntax.option._
    def read(elem: Element): Validated[ParseError, Element] =
      elem.children.collect {
        case e: Element if e.tag == name => e
      }.headOption
        .toValid(MissingElement(Seq(), name))
  }

  class ChoiceElementReader[A](choiceReaders: Seq[XmlReader[_ <: A]]) extends XmlReader[A] {
    val name = s"choice content"
    def read(elem: Element): Validated[ParseError, A] = {
      choiceReaders.map(_.read(elem))
        .reduce(_ orElse _)
        .bimap({
          case m: MissingElement => new MissingElement(m.path, choiceReaders.map(_.name).mkString(" or "))
          case other => other
        },
        identity)
    }
  }

  class ElementReader(tag: String) extends XmlReader[Element] {
    val name = tag

    def read(elem: Element): Validated[ParseError, Element] =
      if (elem.tag == tag) Valid(elem) else Invalid(MissingElement(tag))
  }

  class OptionalAttributeReader[A](val name: String) extends XmlReader[Option[String]] {
    def read(elem: Element): Validated[ParseError, Option[String]] =
      Valid(elem.attributes.find(_.name == name).map(_.values.head))
  }

  class OptionalElementReader(val name: String) extends XmlReader[Option[Element]] {
    def read(elem: Element): Validated[ParseError, Option[Element]] =
      Valid(
      elem.children.collect {
        case e: Element if e.tag == name => e
      }.headOption)
  }

  class SequenceElementReader[A](widgetType: String, min: Int, reader: XmlReader[A]) extends XmlReader[List[A]] {
    import cats.instances.list._

    val name = s"$widgetType sequence content"

    def read(elem: Element): Validated[ParseError, List[A]] = {
      if (elem.tag != widgetType) {
        Invalid(new MissingElement(Seq(widgetType), reader.name))
      } else {
        val childElems = elem.children.collect {
          case e: Element => reader.read(e)
        }.toList
        if (childElems.length < min)
          Invalid(new MissingElement(Seq(widgetType), reader.name))
        else
          Applicative[({ type l[A] = Validated[ParseError, A] })#l].sequence(childElems)
            .bimap({
              case m: MissingElement => new MissingElement(Seq(widgetType), reader.name)
              case other => other
            }, identity)
      }
    }
  }
}

trait XmlReader[+A] {
  def name: String
  def read(elem: Element): Validated[ParseError, A]
  def map[B](f: A => B): XmlReader[B] = new WrappingXmlReader(this, (a: A) => Valid(f(a)))
  def flatMap[B](f: A => Validated[ParseError, B]): XmlReader[B] =
    new WrappingXmlReader(this, (a: A) => f(a))
  def path: Seq[String] = Seq()
  def atPath(path: Seq[String]): XmlReader[A] = new PathedXmlReader(this, path)
  def atPath(path: String): XmlReader[A] = atPath(Seq(path))
}

class WrappingXmlReader[A, B](wrappedReader: XmlReader[A], f: A => Validated[ParseError, B]) extends XmlReader[B] {
  def name = wrappedReader.name

  def read(elem: Element): Validated[ParseError, B] =
    wrappedReader.read(elem).andThen(f)
}

class PathedXmlReader[A](wrappedReader: XmlReader[A], override val path: Seq[String]) extends XmlReader[A] {
  def name = wrappedReader.name
  def read(elem: Element): Validated[ParseError, A] =
    wrappedReader.read(elem).bimap(_.atPath(path), identity)
  override def atPath(newPath: Seq[String]): XmlReader[A] = new PathedXmlReader(this, newPath ++ path)
}
