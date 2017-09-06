// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.fileformat

import
  cats.data.Validated.{ Invalid, Valid }

import
  org.nlogo.core.{ LiteralParser, Model, model },
    model.{ Element, ElementFactory }

import
  org.nlogo.api.{ ComponentSerialization, LabProtocol }

import
  scala.util.{ Failure, Success, Try }

class NLogoXLabFormat(factory: ElementFactory)
  extends ComponentSerialization[NLogoXFormat.Section, NLogoXFormat]
  with LabFormat {
    def parseExperiments(elems: Seq[Element]): Try[Seq[LabProtocol]] = {
      elems.foldLeft(Try(Seq.empty[LabProtocol])) {
        case (Success(acc), e) =>
          LabProtocolXml.read(e) match {
            case Valid(w) => Success(acc :+ w)
            case Invalid(err) => Failure(new NLogoXFormatException(err.message))
          }
        case (failure, e) => failure
      }
    }

    override def deserialize(s: NLogoXFormat.Section): Model => Try[Model] = { (m: Model) =>
      parseExperiments(s.children.collect { case e: Element =>  e })
        .map(protocols => m.withOptionalSection(componentName, Some(protocols), Seq.empty[LabProtocol]))
    }

  def serialize(m: Model): NLogoXFormat.Section =
    factory.newElement("experiments")
      .withElementList(
        m.optionalSectionValue[Seq[LabProtocol]](componentName)
          .getOrElse(Seq())
          .map(p => LabProtocolXml.write(p, factory)))
      .build
}
