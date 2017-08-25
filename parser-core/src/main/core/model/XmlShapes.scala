// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import org.nlogo.core.{ RgbColor, Shape },
  Shape.{ Circle => CoreCircle, Element => CoreElement, Line => CoreLine, LinkLine => CoreLinkLine, LinkShape,
    Polygon => CorePolygon, Rectangle => CoreRectangle, RgbColor, VectorShape }

case class TurtleShape(
  var name: String,
  rotatable: Boolean,
  editableColorIndex: Int,
  elements: Seq[CoreElement]) extends VectorShape {
  }

case class CircleElem(
  color:  RgbColor,
  filled: Boolean,
  marked: Boolean,
  x: Int,
  y: Int,
  diameter: Int) extends CoreCircle

case class LineElem(
  color: RgbColor,
  filled: Boolean,
  marked: Boolean,
  x1: Int,
  y1: Int,
  x2: Int,
  y2: Int) extends CoreLine {
    def startPoint: (Int, Int) = (x1, y1)
    def endPoint: (Int, Int) = (x2, y2)
  }

case class PolygonElem(
  color: RgbColor,
  filled: Boolean,
  marked: Boolean,
  points: Seq[(Int, Int)]) extends CorePolygon

case class RectangleElem(
  color: RgbColor,
  filled: Boolean,
  marked: Boolean,
  x: Int,
  y: Int,
  width: Int,
  height: Int) extends CoreRectangle {
    def upperLeftCorner = (x, y)
    def lowerRightCorner = (x + width, y + height)
  }
