// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import org.nlogo.core.{ RgbColor, Shape },
  Shape.{ Circle => CoreCircle, Element => CoreElement, Line => CoreLine, LinkLine => CoreLinkLine, LinkShape => CoreLinkShape,
    Polygon => CorePolygon, Rectangle => CoreRectangle, RgbColor, VectorShape }

sealed trait XmlShape

object XmlShape {
  def coerce(s: Shape): XmlShape =
    s match {
      case x: XmlShape      => x
      case v: VectorShape   => convertVectorShape(v)
      case l: CoreLinkShape => convertLinkShape(l)
    }

  def convertLinkShape(l: CoreLinkShape): ParsedLinkShape =
    ParsedLinkShape(l.name, l.curviness, l.linkLines.map(convertLinkLine), convertVectorShape(l.indicator))

  def convertVectorShape(v: VectorShape): TurtleShape =
    TurtleShape(v.name, v.rotatable, v.editableColorIndex, v.elements.map(convert))

  private def convertLinkLine(ll: CoreLinkLine): ParsedLinkLine =
    ParsedLinkLine(ll.xcor, ll.isVisible, ll.dashChoices)

  private def convert(e: CoreElement): XmlElement =
    e match {
      case c: CoreCircle => CircleElem(c.color, c.filled, c.marked, c.x, c.y, c.diameter)
      case l: CoreLine => LineElem(l.color, l.filled, l.marked, l.startPoint._1, l.startPoint._2, l.endPoint._1, l.endPoint._2)
      case p: CorePolygon => PolygonElem(p.color, p.filled, p.marked, p.points)
      case r: CoreRectangle =>
        RectangleElem(r.color, r.filled, r.marked, r.upperLeftCorner._1, r.upperLeftCorner._2, r.getWidth, r.getHeight)
    }
}

sealed trait XmlElement extends CoreElement

case class TurtleShape(
  var name: String,
  rotatable: Boolean,
  editableColorIndex: Int,
  elements: Seq[XmlElement]) extends VectorShape with XmlShape

case class CircleElem(
  color:  RgbColor,
  filled: Boolean,
  marked: Boolean,
  x: Int,
  y: Int,
  diameter: Int) extends CoreCircle with XmlElement

case class LineElem(
  color: RgbColor,
  filled: Boolean,
  marked: Boolean,
  x1: Int,
  y1: Int,
  x2: Int,
  y2: Int) extends CoreLine with XmlElement {
    def startPoint: (Int, Int) = (x1, y1)
    def endPoint: (Int, Int) = (x2, y2)
  }

case class PolygonElem(
  color: RgbColor,
  filled: Boolean,
  marked: Boolean,
  points: Seq[(Int, Int)]) extends CorePolygon with XmlElement

case class RectangleElem(
  color: RgbColor,
  filled: Boolean,
  marked: Boolean,
  x: Int,
  y: Int,
  width: Int,
  height: Int) extends CoreRectangle with XmlElement {
    def upperLeftCorner = (x, y)
    def lowerRightCorner = (x + width, y + height)
  }

case class ParsedLinkShape(
  var name:  String,
  curviness: Double,
  linkLines: Seq[ParsedLinkLine],
  indicator: TurtleShape) extends CoreLinkShape with XmlShape

case class ParsedLinkLine(
  xcor:        Double,
  isVisible:   Boolean,
  dashChoices: Seq[Float]) extends CoreLinkLine
