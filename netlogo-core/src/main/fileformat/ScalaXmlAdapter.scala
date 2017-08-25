// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.fileformat

import org.nlogo.core.model.{ Attribute, Element, ElementBuilder, ElementFactory, Node, Text }

import scala.xml.{ Atom, Attribute => XmlAttribute, Elem, Node => XmlNode, Null, PCData, Text => XmlText, TopScope, UnprefixedAttribute }

case class ScalaXmlElement(val elem: Elem) extends Element {
  def tag: String = elem.label
  def attributes: Seq[Attribute] = elem.attributes.map {
    case a: XmlAttribute => new ScalaXmlAttribute(a)
  }.toSeq
  def children: Seq[Node] = elem.child.collect {
    case e: Elem => new ScalaXmlElement(e)
    case t: XmlText => new ScalaTextElement(t)
    case c: PCData => new ScalaTextElement(c)
  }
}

case class ScalaXmlAttribute(val attr: XmlAttribute) extends Attribute {
  def name: String = attr.key
  def value: String = attr.value.map(t => t match { case t: XmlText => t.data }).mkString("")
}

case class ScalaTextElement(val atom: Atom[String]) extends Text {
  def text: String = atom.data
}

object ScalaXmlElementFactory extends ElementFactory {
  def newElement(tag: String): ElementBuilder = new ScalaXmlElementBuilder(tag, Seq(), Seq())
}

case class ScalaXmlElementBuilder(tag: String, attributes: Seq[ScalaXmlAttribute], elements: Seq[Either[ScalaXmlElement, ScalaTextElement]])
  extends ElementBuilder {
  def withAttribute(name: String, value: String): ElementBuilder =
    copy(attributes = attributes :+ new ScalaXmlAttribute(new UnprefixedAttribute(name, value, Null)))
  def withElement(element: Element): ElementBuilder =
    element match {
      case e: ScalaXmlElement => copy(elements = elements :+ Left(e))
      case _ => this
    }
  def withText(text: String): ElementBuilder =
    copy(elements = elements :+ Right(new ScalaTextElement(new scala.xml.PCData(text))))
  def build: Element = {
    val finalAttrs =
      if (attributes.isEmpty) Null
      else if (attributes.length == 1) attributes.head.attr
      else attributes.reduceLeft[ScalaXmlAttribute] {
        case (a1: ScalaXmlAttribute, a2: ScalaXmlAttribute) => new ScalaXmlAttribute(a2.attr.copy(a1.attr))
      }.attr
    val finalElems: Seq[XmlNode] =
      elements.map(_.fold(se => se.elem, st => new scala.xml.PCData(st.text)))
    new ScalaXmlElement(Elem(null: String, tag, finalAttrs, TopScope, true, finalElems: _*))
  }
}
