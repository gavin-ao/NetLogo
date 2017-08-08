// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

sealed trait Node

trait Text extends Node {
  def text: String
}

trait Element extends Node {
  def tag: String
  def attributes: Seq[Attribute]
  def children: Seq[Node]
}

trait Attribute {
  def name: String
  def values: Seq[String]
}

trait ElementBuilder {
  def withAttribute(name: String, value: String): ElementBuilder
  def withElement(element: Element): ElementBuilder
  def withText(text: String): ElementBuilder
  def build: Element
}
