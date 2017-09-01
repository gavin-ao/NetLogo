// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.core.model

import
  org.nlogo.core.{ RgbColor, Shape }, Shape.{ Element => CoreElement }

import
  org.scalatest.FunSuite

object ShapeXmlTest {
  import DummyXML._

  val emptyTurtleXml = Elem("turtleShape",
    Seq(Attr("name", "default"), Attr("rotatable", "true"), Attr("editableColorIndex", "0")),
    Seq(Elem("elements", Seq(), Seq())))

  val emptyTurtleShape = TurtleShape("default", true, 0, Seq())

  val circle = CircleElem(RgbColor(1, 2, 3), true, false, 5, 10, 20)
  val rectangle = RectangleElem(RgbColor(1, 2, 3), true, false, 5, 10, 20, 40)
  val polygon = PolygonElem(RgbColor(1, 2, 3), true, false, Seq((1, 2), (4, 8), (16, 32)))
  val line = LineElem(RgbColor(1, 2, 3), false, false, 5, 10, 20, 40)

  val circleXml = Elem("circle",
    Seq(Attr("cx", "5"), Attr("cy", "10"), Attr("diameter", "20"),
      Attr("color", "#010203"), Attr("filled", "true"), Attr("marked", "false")),
    Seq())
  val lineXml = Elem("line",
    Seq(Attr("x1", "5"), Attr("y1", "10"), Attr("x2", "20"), Attr("y2", "40"),
      Attr("color", "#010203"), Attr("filled", "false"), Attr("marked", "false")),
    Seq())
  val polygonXml = Elem("polygon",
    Seq(Attr("points", "1,2 4,8 16,32"),
      Attr("color", "#010203"), Attr("filled", "true"), Attr("marked", "false")),
    Seq())
  val rectangleXml = Elem("rect",
    Seq(Attr("x", "5"), Attr("y", "10"), Attr("width", "20"), Attr("height", "40"),
      Attr("color", "#010203"), Attr("filled", "true"), Attr("marked", "false")),
    Seq())

  val circleTurtle = shapeWith(circle)
  val circleTurtleXml = xmlWith(circleXml)
  val lineTurtle = shapeWith(line)
  val lineTurtleXml = xmlWith(lineXml)
  val polyTurtle = shapeWith(polygon)
  val polyTurtleXml = xmlWith(polygonXml)
  val rectTurtle = shapeWith(rectangle)
  val rectTurtleXml = xmlWith(rectangleXml)

  val multiShapeTurtle =
    emptyTurtleShape.copy(elements = Seq(circle, line, polygon, rectangle))

  val multiShapeTurtleXml =
    emptyTurtleXml.copy(children = Seq(Elem("elements", Seq(), Seq(circleXml, lineXml, polygonXml, rectangleXml))))

  val otherLineTurtle = {
    import org.nlogo.core.ShapeParser._
    VectorShape("default", true, 0, Line(RgbColor(1, 2, 3), false, (5, 10), (20, 40)))
  }

  // offset is typically one of { -0.2, 0.0, 0.2 }
  def linkLineXml(offset: Double): Elem =
    Elem("line",
      Seq(Attr("offset", offset.toString), Attr("isVisible", "true"), Attr("stroke-dasharray", "1.0,0.0")),
      Seq())

  def linkLine(offset: Double): ParsedLinkLine =
    ParsedLinkLine(offset, true, Seq(1.0f, 0.0f))

  val linkShapeXml =
    Elem("linkShape",
      Seq(Attr("name", "foo"), Attr("curviness", "0.0")),
      Seq(linkLineXml(-0.2), linkLineXml(0.0), linkLineXml(0.2), rectTurtleXml.copy(tag = "indicator")))

  val linkShape =
    ParsedLinkShape("foo", 0, Seq(linkLine(-0.2), linkLine(0.0), linkLine(0.2)), rectTurtle)

  def shapeWith(e: XmlElement, t: TurtleShape = emptyTurtleShape): TurtleShape =
    t.copy(elements = t.elements :+ e)

  def xmlWith(e: Elem): Elem =
    emptyTurtleXml.copy(children = Seq(Elem("elements", Seq(), Seq(e))))
}

class ShapeXmlTest extends FunSuite {
  import DummyXML._
  import ShapeXmlTest._

  def readFromXml(x: Elem): Shape =
    ShapeXml.read(x).toOption.get

  def writeToXml(s: Shape): Element =
    ShapeXml.write(s, Factory)

  test("reads empty turtle shape") {
    assertResult(emptyTurtleShape)(readFromXml(emptyTurtleXml))
  }

  test("writes empty turtle shape") {
    assertResult(emptyTurtleXml)(writeToXml(emptyTurtleShape))
  }

  test("reads turtle shape with circle") {
    assertResult(circleTurtle)(readFromXml(circleTurtleXml))
  }

  test("writes turtle shape with circle") {
    assertResult(circleTurtleXml)(writeToXml(circleTurtle))
  }

  test("reads turtle shape with rectangle") {
    assertResult(rectTurtle)(readFromXml(rectTurtleXml))
  }

  test("writes turtle shape with rectangle") {
    assertResult(rectTurtleXml)(writeToXml(rectTurtle))
  }

  test("reads turtle shape with line") {
    assertResult(lineTurtle)(readFromXml(lineTurtleXml))
  }

  test("writes turtle shape with line") {
    assertResult(lineTurtleXml)(writeToXml(lineTurtle))
  }

  test("reads turtle shape with polygon") {
    assertResult(polyTurtle)(readFromXml(polyTurtleXml))
  }

  test("writes turtle shape with polygon") {
    assertResult(polyTurtleXml)(writeToXml(polyTurtle))
  }

  test("reads turtle shape with multiple shapes") {
    assertResult(multiShapeTurtle)(readFromXml(multiShapeTurtleXml))
  }

  test("writes turtle shape with multiple shapes") {
    assertResult(multiShapeTurtleXml)(writeToXml(multiShapeTurtle))
  }

  test("reads link shapes") {
    assertResult(linkShape)(readFromXml(linkShapeXml))
  }

  test("writes link shapes") {
    assertResult(linkShapeXml)(writeToXml(linkShape))
  }

  test("writes turtle shape for other types of TurtleShapes") {
    assertResult(lineTurtleXml)(writeToXml(otherLineTurtle))
  }
}
