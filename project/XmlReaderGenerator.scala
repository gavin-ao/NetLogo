import java.io.File
import sbt._
import Keys._
import NetLogoBuild.autogenRoot

import scala.xml.{ Attribute, Elem, Node, NodeSeq, Text, XML }

object XmlReaderGenerator {
  lazy val settings = Seq(sourceGenerators in Compile += task.taskValue)

  lazy val task =
    Def.task {
      /*
      // Note: this is only suitable for a single file at the moment, but could probably be adapted to make several readers
      val cachedEvents =
        FileFunction.cached(streams.value.cacheDirectory / "xmlReader", inStyle = FilesInfo.hash, outStyle = FilesInfo.hash) {
          (in: Set[File]) =>
            Set(generator(streams.value.log.info(_),
              autogenRoot.value / "fileformat" / "netlogo.xsd",
              (sourceManaged in Compile).value,
              "org.nlogo.core.model"))
        }
      cachedEvents(Set(autogenRoot.value / "fileformat" / "netlogo.xsd")).toSeq
      */

     Seq(
       generator(streams.value.log.info(_),
         autogenRoot.value / "fileformat" / "netlogo.xsd",
         (sourceManaged in Compile).value,
         "org.nlogo.core.model",
         "Widget",
         miscNames ++ widgetNames,
         widgetNames,
         // TODO: We now have an additionalImports appInfo which would let us abandon this entirely
        "import org.nlogo.core.{ AgentKind, Widget, Chooseable, Direction, Horizontal, UpdateMode, Vertical }"),
       generator(streams.value.log.info(_),
         autogenRoot.value / "fileformat" / "netlogo.xsd",
         (sourceManaged in Compile).value,
         "org.nlogo.core.model",
         "Shape",
         shapeNames,
         Seq("turtleShape", "linkShape"),
         "import org.nlogo.core.Shape.{ Element => CoreElement }"
         )
     )
    }

    val shapeNames = Seq("circle", "elements", "line", /* "linkLine", "linkShape", */ "polygon", "rect", "turtleShape")
    val miscNames = Seq("choices", "chooseable", "dimensions", "listChoice", "listContents", "numericData", "pen", "stringData")
    val widgetNames = Seq("button", "chooser", "numericInput", "monitor", "output", "plot", "slider", "switch", "textbox", "view", "stringInput")

    // TODO: Support default values for attributes

    def generator(log: String => Unit, source: File, dir: File, ppackage: String, baseType: String, elems: Seq[String], topTypes: Seq[String], additionalImports: String): File = {
      val file = ppackage.split('.').foldLeft(dir / "org" / "nlogo")(_ / _) / s"${baseType}Xml.scala"
      log("creating: " + file)

      val complexTypeSpecs = parseFile(source, elems, topTypes)

      val codeString = new StringBuilder()
      def append(s: String) = codeString.append(s + "\n")

      implicit val typesMap = complexTypeSpecs.map(t => t.name -> t).toMap

      append(s"package $ppackage")

      val classTree: Seq[Either[String, (String, String)]] =
        complexTypeSpecs.flatMap(_.importNames).map {
          case i =>
            if (! i.contains(".")) Left(i)
            else {
              val parts = i.split("\\.")
              Right(parts.head, parts.tail.mkString(""))
            }
        }
      val initialImports = classTree.map(_.fold(identity, _._1))
      val followImports: Seq[(String, Seq[String])] = {
        val subroots = classTree.flatMap(_.fold(_ => Seq(), t => Seq(t._1))).distinct
        val subrootMap: Map[String, Seq[String]] = subroots.map(r => r -> Seq()).toMap
        classTree.foldLeft(subrootMap) {
          case (acc, Left(_)) => acc
          case (acc, Right((r, n))) => acc + (r -> (acc(r) :+ n))
        }.toSeq
      }
      append(additionalImports)
      if (initialImports.nonEmpty) {
        append(s"import org.nlogo.core.${initialImports.distinct.mkString("{ ", ", ", " }")}${if (followImports.nonEmpty) "," else ""}")
        followImports
          .map { case (root, klassNames) => s"  ${root}.${klassNames.mkString("{ ", ", ", " }")}" }
          .foreach(append)
      }
      append("import cats.Apply")
      append("import cats.data.Validated, Validated.{ Valid, Invalid }")

      append("")
      append(s"object ${baseType}Xml {")
      append(s"  trait Reader { def read(xml: Element): Validated[ParseError, ${baseType}] }")
      append(s"  trait Writer[W <: ${baseType}] { def write(${baseType.toLowerCase}: W, factory: ElementFactory): Element }")

      complexTypeSpecs.map(generateComplexTypeReader(typesMap)).foreach(append)
      append("  val readers = Map[String, Reader](")
      append(complexTypeSpecs.filter(_.isTopType).map(s => s"""    "${s.name}" -> ${s.name.capitalize}Reader""").mkString(",\n"))
      append("  )")

      append(s"  def read(xml: Element): Validated[ParseError, ${baseType}] = {")
      append("    readers.get(xml.tag).map(_.read(xml))")
      append("     .getOrElse(Invalid(UnknownWidgetType(Seq(xml.tag))))")
      append("  }")

      append("")

      complexTypeSpecs.map(generateComplexTypeWriter(typesMap)).foreach(append)
      append(s"  def write(${baseType.toLowerCase}: ${baseType}, factory: ElementFactory): Element = {")
      matchCase(baseType.toLowerCase, generateComplexTypeWriteMatches(complexTypeSpecs), indentLevel = 4).foreach(append)
      append("  }")

      append("}")

      IO.write(file, codeString.toString)
      file
    }

    // Todo notes:
    // * The names for methods / helper classes could be improved
    // * There is a great deal of repetition in the code and it seems poorly organized. This indicates a refactor is in order
    //   Potential axes for abstraction:
    //   * type coercion
    //   * all / choice / seq / complexType
    //   * processReadElement contains a recursive call into generateContentReader, but ends up having to go from a structured Decl
    //     datatype into a non-structured string datatype. How can we avoid losing information in this operation.
    //   * abstraction over identity / map / flatMap (coerceToString does this a bit, perhaps we could use it in more places?)
    //
    //   Axes of abstraction:
    //    * attribute / element
    //    * types
    //    * optional / mandatory
    //    * sequence / single
    //  * Put autogenerated warning at top of file
    //  * Potentially - move boilerplate to separate text file
    //
    // Notes for tuesday:
    // * Still need to figure out link shapes
    // * we *could* redesign the types of classes produced by this to allow instantiation of pass-through
    //   sequence readers/writers wherein those readers/writers took the name of sequence element
    //   they were parsing. This would allow for elements to be properly renamed.
    //   e.g. right now `<element name="fooBar" type="foo">` won't work because FooReader expects
    //   the element passed in to have tag "foo". This certainly has potential applications beyond
    //   sequence readers, I've just noticed it most looking at sequence readers, since those seem to
    //   be the primary "pass-through readers
    // * figure out how to deal with Shapes being written which are not specifically the type read
    //
    // Idea: SequenceReader[A], have it return Valid[(A, Seq[Element])] - if it's successful it returns it's return type and
    // the elements it *did not* consume. This is probably flexible enough to accomodate the general sequence-reading case.

    // AttributeGroup represents an xml schema attribute group
    case class AttributeGroup(name: String, attributes: Seq[SpecifiedAttribute])

    case class SpecifiedAttribute(name: String, fieldName: String, tpe: DataType, fieldTpe: DataType, required: Boolean)

    // complexType represents a fully-formed complexType declaration
    case class ComplexType(name: String, isTopType: Boolean, content: ComplexTypeContent) {
      def importNames = content.importNames
      def klassName = content.klassName
    }

    // klassName *might* belong here as well
    case class ComplexTypeContent(attributes: Seq[SpecifiedAttribute], elements: ComplexTypeElements, klassName: String, isPassThrough: Boolean, importNames: Seq[String] = Seq())

    sealed trait DataType {
      def attributeReaderName: Option[String]
      def className: String
      def resolvedType(implicit types: Map[String, ComplexType]): String =
        this match {
          case DataType.DeferredType(name) => types(name).klassName
          case other => other.className
        }
    }

    object DataType {
      def fromName(s: String): DataType = {
        val namesToType = Map[String, DataType](
          "xsd:boolean"            -> Boolean,
          "svg:ColorType"          -> Color,
          "svg:PointsType"         -> Points,
          "xsd:integer"            -> Integer,
          "xsd:positiveInteger"    -> Integer,
          "xsd:nonNegativeInteger" -> Integer,
          "xsd:string"             -> String,
          "xsd:double"             -> Double
        )

        namesToType.getOrElse(s, DeferredType(s))
      }
      case object Boolean extends DataType {
        val attributeReaderName = Some("booleanReader")
        val className = "Boolean"
      }
      case object Character extends DataType {
        val attributeReaderName = Some("characterReader")
        val className = "Char"
      }
      case object Color extends DataType {
        val attributeReaderName = Some("colorReader")
        val className = "RgbColor"
      }
      case object Points extends DataType {
        val attributeReaderName = Some("pointsReader")
        val className = "Seq[(Int, Int)]"
      }
      case object Double extends DataType {
        val attributeReaderName = Some("doubleReader")
        val className = "Double"
      }
      case object Integer extends DataType {
        val attributeReaderName = Some("intReader")
        val className = "Int"
      }
      case object String extends DataType {
        val attributeReaderName = Some("stringReader")
        val className = "String"
      }
      case class Boxed(base: DataType) extends DataType {
        val attributeReaderName = None
        val className =
          base match {
            case DataType.Boolean => "java.lang.Boolean"
            case DataType.Double  => "java.lang.Double"
            case _                => "AnyRef"
          }
      }
      case class RestrictionType(base: DataType, resultType: String, restriction: Restriction) extends DataType {
        val attributeReaderName = {
          restriction match {
            case Restriction.Enum(options) =>
              val enumMap = options.map {
                case (k, v) => s""""$k" -> $v"""
              }.mkString("Map(", ", ", ")")
              Some(s"enumReader[$resultType](${enumMap})")
            case _ => None
          }
        }
        val className = resultType
      }
      case class DeferredType(name: String) extends DataType {
        val attributeReaderName = None
        val className = "AnyRef"
      }
      case class NestedComplexType(content: ComplexTypeContent) extends DataType {
        val attributeReaderName = None
        val className = "AnyRef" // if we add klassName to ComplexTypeContent, use that
      }
    }

    sealed trait Restriction
    object Restriction {
      case class Enum(options: Seq[(String, String)]) extends Restriction
    }

    sealed trait SequenceChild

    // Element definitions
    sealed trait ElementDefinition extends SequenceChild {
      def min: Int
      def max: Option[Int]
    }
    case class SpecifiedElement(name: String, fieldName: String, additionalField: Option[String], tpe: DataType, min: Int, max: Option[Int]) extends ElementDefinition
    // NOTE: it would be possible to add additionalField to ComplexTypeReference, but that hasn't yet been done
    case class ComplexTypeReference(refName: String, fieldName: Option[String], min: Int, max: Option[Int]) extends ElementDefinition

    // Complex types contain one of all, choice, sequence, or simpleContent
    sealed trait ComplexTypeElements {
      def elements: Seq[ElementDefinition]
    }
    case class All(elements: Seq[ElementDefinition]) extends ComplexTypeElements
    case class Choice(elements: Seq[ElementDefinition], fieldName: String, klassName: Option[String], isPassThrough: Boolean = false) extends ComplexTypeElements with SequenceChild
    // NOTE: At the moment we only support reading homogenous sequences
    case class Sequence(child: SequenceChild, fieldName: Option[String], variadic: Boolean) extends ComplexTypeElements {
      def elements =
        child match {
          case elem: ElementDefinition => Seq(elem)
          case _ => Seq()
        }
    }
    case class SimpleContent(baseType: DataType, fieldName: String) extends ComplexTypeElements {
      def elements = Seq()
    }

    // these classes represent program statements
    case class Decl(name: String, assignedValue: String)
    case class Assignment(readerName: String, fieldNames: Seq[String], varName: String, variadic: Boolean = false)
    case class CoercionFunction(name: String, inputs: Seq[(String, String)], outputType: String, lines: Seq[String])

    implicit class RichNodeSeq(nodeSeq: NodeSeq) {
      def appInfo(propName: String): Option[String] =
        (nodeSeq \ "annotation" \ "appinfo" \ s"@$propName").headOption.map(_.text)
    }

    trait DeclGenerator {
      def methodIndentLevel: Int
      var decls: Seq[Decl] = Seq.empty[Decl]
      def declStrings: Seq[String] =
        decls.map(d => (" " * (methodIndentLevel + 2)) + s"val ${d.name} = ${d.assignedValue}")
      def declare(name: String, assignedValue: String): Unit =
        decls = decls :+ Decl(name, assignedValue)
      def declare(decl: Decl): Unit =
        decls = decls :+ decl
    }

    trait AssignmentGenerator {
      def methodIndentLevel: Int
      def complexType: ComplexType
      var assignmentMap: Map[String, Assignment] = scala.collection.immutable.ListMap.empty[String, Assignment]
      // the last assignment takes precedence
      def assignment(readerName: String, fieldNames: Seq[String], varName: String, variadic: Boolean = false): Unit = {
        assignmentMap = assignmentMap + (varName -> Assignment(readerName, fieldNames, varName, variadic))
      }
      def assignments = assignmentMap.values.toSeq
      def assignmentStrings: Seq[String] = {
        val renderedAssignments =
          assignments.flatMap(a => a.fieldNames.map { f =>
            if (a.variadic) s"${f} = ${a.varName}: _*"
            else s"${f} = ${a.varName}"
          }).mkString(", ")

        if (assignments.isEmpty) Seq("null")
        else if (complexType.content.isPassThrough) Seq((" " * (methodIndentLevel + 2)) + s"${assignments.head.readerName}.read(xml)")
        else {
          val addPath =
            if (complexType.content.elements.isInstanceOf[Choice]) ""
            else s""".bimap(_.atPath("${complexType.name}"), identity _)"""
          val applyMap =
            if (assignments.length == 1) "Apply[ReadValidation#l].map"
            else s"Apply[ReadValidation#l].map${assignments.length}"
          Seq(
            s"$applyMap(",
            s"  ${assignments.map(_.readerName).map(r => s"${r}.read(xml)").mkString(", ")}",
             ") {",
            s"  case (${assignments.map(_.varName).mkString(", ")}) =>",
            s"    ${complexType.klassName}(${renderedAssignments})",
            s"  }$addPath").map(l => (" " * methodIndentLevel) + l)
        }

      }
    }

    trait CoercionGenerator {
      def methodIndentLevel: Int
      var coercions = Seq.empty[CoercionFunction]
      def coerce(name: String, inputs: Seq[(String, String)], outputType: String, lines: Seq[String]): Unit =
        coercions = coercions :+ CoercionFunction(name, inputs, outputType, lines)
      def coercionFunctionToString(indentLevel: Int)(f: CoercionFunction): Seq[String] = {
        ((s"def ${f.name}(${f.inputs.map(t => s"${t._1}: ${t._2}").mkString(", ")}): ${f.outputType} = {" +: f.lines.map(indent(2))) :+
          "}").map(indent(indentLevel))
      }
      def coercionStrings = coercions.flatMap(coercionFunctionToString(methodIndentLevel))
    }

    trait WriterComponentGenerator {
      def methodIndentLevel: Int
      def elementName: String
      var writers = Seq.empty[String]
      def write(s: String): Unit = {
        writers = writers :+ s
      }
      def writerStrings =
        indent(methodIndentLevel + 2)(s"""factory.newElement("${elementName}")""") +:
          writers.map(indent(methodIndentLevel + 4)) :+
          indent(methodIndentLevel + 4)(".build")
    }

    class ReaderGenerator(val complexType: ComplexType, val methodIndentLevel: Int = 2) extends DeclGenerator with AssignmentGenerator {
      val (base, name) =
        if (complexType.isTopType) ("Reader", "")
        else (s"XmlReader[${complexType.klassName}]", s"""val name = "${complexType.name}"""")

      val widgetName = complexType.name.capitalize

      def buildReader: String =
        s"""|  object ${widgetName}Reader extends ${base} {
            |    type ReadValidation = ({ type l[A] = Validated[ParseError, A] })
            |    $name
            |    def read(xml: Element): Validated[ParseError, ${complexType.klassName}] = {
            |${declStrings.mkString("\n")}
            |${assignmentStrings.mkString("\n")}
            |    }
            |  }""".stripMargin
    }

    class WriterGenerator(val complexType: ComplexType, val methodIndentLevel: Int = 2) extends DeclGenerator with CoercionGenerator with WriterComponentGenerator {
      val widgetName = complexType.name.capitalize
      val elementName = complexType.name
      val base =
        if (complexType.isTopType) s"extends Writer[${complexType.klassName}] "
        else s""

      var additionalWriters = Seq.empty[WriterGenerator]

      def withAdditionalWriter(w: WriterGenerator) =
        additionalWriters = additionalWriters :+ w

      def buildWriter: String =
        s"""|  object ${widgetName}Writer ${base}{
            |${coercionStrings.mkString("\n")}
            |    def write(w: ${complexType.klassName}, factory: ElementFactory): Element = {
            |${declStrings.mkString("\n")}
            |${writerStrings.mkString("\n")}
            |    }
            |  }""".stripMargin ++ additionalWriters.map(_.buildWriter).mkString("\n\n", "\n\n", "")
    }

    def indent(i: Int)(s: String): String =
      (" " * i) + s

    def matchCase(e: String, cases: Seq[(String, String, String)], indentLevel: Int = 0): Seq[String] =
      (s"$e match {" +: cases.map(matchCaseString).map(indent(2)) :+ "}").map(indent(indentLevel))

    def matchCaseString(t: (String, String, String)): String =
      t match {
        case (v, tpeName, exp) => s"case $v: $tpeName => $exp"
      }

    def generateAttributeReader(attr: SpecifiedAttribute): String = {
      val readerName =
        attr.tpe.attributeReaderName.getOrElse(throw new Exception(s"unsupported attribute type: ${attr.tpe}"))
      val base = s"""XmlReader.${readerName}("${attr.name}")"""
      if (attr.fieldTpe != attr.tpe) s"${base}.map(${conversion(attr.tpe, attr.fieldTpe)})"
      else base
    }

    def conversion(sourceTpe: DataType, destTpe: DataType): String = {
      (sourceTpe, destTpe) match {
        case (DataType.Color, DataType.Double) => "XmlReader.rgbColorToDouble"
        case _ => throw new Exception(s"unsupported conversion from ${sourceTpe} to ${destTpe}")
      }
    }

    def generateAllElementReader(elem: SpecifiedElement)(implicit types: Map[String, ComplexType], generator: ReaderGenerator): String = {
      val baseReader =
        if (elem.min == 0) s"""XmlReader.optionalElementReader("${elem.name}")""" // optional
        else               s"""XmlReader.allElementReader("${elem.name}")""" // single

      val processMatch = processReadElement(elem.name, elem.min, elem.tpe, generator)
      baseReader + processMatch
    }

    def generateSingleElementReader(elemName: String, min: Int, tpe: DataType)(implicit types: Map[String, ComplexType], generator: ReaderGenerator): String = {
      val processMatch = processReadElement(elemName, min, tpe, generator)

      s"""XmlReader.elemReader("${elemName}")""" + processMatch
    }

    def processReadElement(elemName: String, min: Int, tpe: DataType, generator: ReaderGenerator)(implicit types: Map[String, ComplexType]): String = {
      (min, tpe) match {
        case (0, DataType.String)             => ".map(_.flatMap(e => XmlReader.textToOption(XmlReader.childText(e))))"
        case (_, DataType.String)             => ".map(XmlReader.childText _)"
        case (_, DataType.DeferredType(name)) => s".flatMap(${name.capitalize}Reader.read)"
        case (_, DataType.NestedComplexType(content)) =>
          generateContentReader(elemName, content, generator)
          s".flatMap(${generator.decls.last.name}.read _)"
        case (_, other)                       => s".map(XmlReader.childText _).flatMap(${stringToType(elemName, other)})"
        case _ => throw new Exception("Don't know how to read element: " + elemName + " of type " + tpe)
      }
    }

    def stringToType(elemName: String, childType: DataType) = {
      val invalidation = s"""Invalid(new InvalidElement(Seq(), "${elemName}", s))"""
      def conversion(tpe: DataType): (String => String) =
        tpe match {
          case DataType.Boolean  => { (varName: String) => s"$varName.toBoolean" }
          case DataType.Double   => { (varName: String) => s"$varName.toDouble" }
          case DataType.Boxed(t) => { (varName: String) => s"${t.className}.box(${conversion(t)(varName)})" }
          case other             => throw new Exception("Unsupported stringToType conversion for: " + other)
        }
      s"{ (s: String) => try { Valid(${conversion(childType)("s")}) } catch { case e: Exception => $invalidation } }"
    }

    // returns name of choice reader
    def declareChoiceReader(c: Choice, name: String)(implicit types: Map[String, ComplexType], generator: ReaderGenerator): String = {
     // case class Choice(elements: Seq[ElementDefinition], fieldName: String, klassName: Option[String], isPassThrough: Boolean = false) extends ComplexTypeElements with SequenceChild
      val subReaderDecls = c.elements.map(declareRequiredElementReader)
      val convert = c.klassName.filterNot(_ == "AnyRef").map(n => s".map($n(_))").getOrElse("")
      generator.declare(s"${name}ChoiceReader",
        s"""XmlReader.choiceElementReader(Seq(${subReaderDecls.mkString(", ")}))${convert}""")
      s"${name}ChoiceReader"
    }

    def declareRequiredElementReader(e: ElementDefinition)(implicit types: Map[String, ComplexType], generator: ReaderGenerator): String =
      e match {
        case se: SpecifiedElement =>
          generator.declare(s"${se.name}Reader", generateSingleElementReader(se.name, se.min, se.tpe))
          s"${se.name}Reader"
        case ctr: ComplexTypeReference =>
          generator.declare(s"${ctr.refName}SubReader", s"${ctr.refName.capitalize}Reader")
          s"${ctr.refName}SubReader"
      }

    def generateContentReader(name: String, content: ComplexTypeContent, generator: ReaderGenerator)(implicit types: Map[String, ComplexType]): Unit = {
      implicit val gen = generator
      content.elements match {
        case All(es) =>
          val elems = content.elements.elements.map(resolveElement(types))
          (elems.map(elem => generator.declare(s"${elem.name}Reader", generateAllElementReader(elem))),
            elems.map { e =>
              val varNames = Seq(e.fieldName) ++ e.additionalField.map(f => Seq(f)).getOrElse(Seq.empty[String])
              generator.assignment(s"${e.name}Reader", varNames, e.name)
            })
        case c: Choice =>
          generator.assignment(declareChoiceReader(c, name), Seq(c.fieldName), name)
        case Sequence(e, fieldName, variadic) =>
          e match {
            case e: ElementDefinition => declareRequiredElementReader(e)
            case c: Choice            => declareChoiceReader(c, name)
          }
          val finalFieldName = fieldName.getOrElse(e match {
            case se: SpecifiedElement => se.fieldName
            case ctr: ComplexTypeReference => resolveElement(types)(ctr).fieldName
            case c: Choice => name
          })
          val min = e match {
            case ed: ElementDefinition => ed.min
            case _ => 0
          }
          generator.declare(s"${name}SequenceReader",
            s"""XmlReader.sequenceElementReader("${name}", ${min}, ${generator.decls.last.name})""")
          generator.assignment(s"${name}SequenceReader", Seq(finalFieldName), name, variadic)
        case SimpleContent(baseType: DataType, fieldName) =>
          generator.declare(s"${name}ContentReader", generateSingleElementReader(name, 1, baseType))
          generator.assignment(s"${name}ContentReader", Seq(fieldName), name)
      }
    }

    def generateComplexTypeWriter(typesMap: Map[String, ComplexType])(spec: ComplexType): String = {
      implicit val tpes = typesMap
      implicit val generator = new WriterGenerator(spec, 4)
      spec.content.attributes.foreach(a => generateAttributeWriter(a))
      generateElementsWriter(spec.content, "w")
      generator.buildWriter
    }

    // things to refactor:
    //   * we can remove WriterElements entirely once we handle the "All" case in generateElementsWriter above

    // decls list subelements created and built before the final builder,
    // string gives the call made on the final builder
    def generateElementsWriter(content: ComplexTypeContent, varName: String)(implicit types: Map[String, ComplexType], generator: WriterGenerator): Unit = {
      content.elements match {
        case SimpleContent(tpe, fieldName) =>
          generator.write(s".withText(${varName}.${fieldName}.toString)")
        case All(es) =>
          es.map(resolveElement(types))
            .map(e => specifiedElementWriter(s"${varName}.${e.fieldName}")(e))
            .foreach {
              case (decl, writer) =>
                generator.declare(decl)
                generator.write(writer)
            }
        case Sequence(e: ElementDefinition, _, _) =>
          val resolvedElem = resolveElement(types)(e)
          val (decl, _) = specifiedElementWriter(s"v")(resolvedElem)
          val declaredName = s"${resolvedElem.fieldName}Elem"
          val variable = if (content.isPassThrough) varName else s"${varName}.${resolvedElem.fieldName}"
          generator.declare(declaredName, s"${variable}.map(v => ${decl.assignedValue})")
          generator.write(s".withElementList($declaredName)")
        case Sequence(c: Choice, _, _) =>
          val fn = choiceCoercionFunction(c)
          generator.write(s".withElementList(${varName}.map(e => ${fn}(e, factory)))")
        case c: Choice =>
          val fn = choiceCoercionFunction(c)
          generator.declare(s"${c.fieldName}Elem", s"${fn}(${varName}, factory)")
          generator.write(s".withElement(${c.fieldName}Elem)")
        case other =>
      }
    }

    private def choiceCoercionFunction(c: Choice)(implicit types: Map[String, ComplexType], generator: WriterGenerator): String = {
      val matchCases = c.elements.map { elem =>
        val resolvedElem = resolveElement(types)(elem)
        val className = resolvedElem.tpe.resolvedType
        (resolvedElem.name, className, specifiedElementWriter(resolvedElem.name)(resolvedElem)._1.assignedValue)
      }
      val matchAccessor = if (c.isPassThrough) "v" else s"v.${c.fieldName}"
      generator.coerce(s"${c.fieldName}ToElem",
        Seq(("v", c.klassName.getOrElse("AnyRef")), ("factory", "ElementFactory")),
        "Element", matchCase(matchAccessor, matchCases))
      s"${c.fieldName}ToElem"
    }

    private def specifiedElementWriter(varName: String)(e: SpecifiedElement)(implicit types: Map[String, ComplexType], generator: WriterGenerator): (Decl, String) = {
      if (e.min == 0 && e.max == Some(1)) { // verify optional type
        val decl = specificWriter("t", e.name, e.tpe)
        (Decl(s"${e.name}OptionElem", s"${varName}.map(t => ${decl.assignedValue})"),
          s".withOptionalElement(${e.name}OptionElem)")
      } else {
        val decl = specificWriter(varName, e.name, e.tpe)
        (decl, s".withElement(${decl.name})")
      }
    }

    private def specificWriter(varName: String, name: String, tpe: DataType)(implicit types: Map[String, ComplexType], generator: WriterGenerator): Decl = {
      def generateAndAddElement(text: String): Decl =
        Decl(s"${name}Elem", newElement(text))
      def writeType(name: String, klassName: String): Decl =
        Decl(s"${name}Elem", s"${name.capitalize}Writer.write(${varName}.asInstanceOf[${klassName}], factory)")
      def newElement(text: String): String =
        s"""factory.newElement("${name}").withText(${text}).build"""
      tpe match {
        case DataType.String                                    => generateAndAddElement(varName)
        case DataType.Boxed(DataType.Double | DataType.Boolean) => generateAndAddElement(s"${varName}.toString")
        case DataType.DeferredType(tName)        => writeType(tName, types(tName).klassName)
        case DataType.NestedComplexType(content) =>
          val subTypeGenerator = new WriterGenerator(ComplexType(s"${name}", false, content), generator.methodIndentLevel)
          generator.withAdditionalWriter(subTypeGenerator)
          generateElementsWriter(content, "w")(types, subTypeGenerator)
          writeType(name, content.klassName)
        case _ => Decl(s"${name}Elem", "null")
      }
    }

    private def generateAttributeWriter(a: SpecifiedAttribute)(implicit generator: WriterGenerator): Unit = {
      val value = s"w.${a.fieldName}"
      def toResult(baseCoercion: (String => String)): String = {
        if (a.required) baseCoercion(value)
        else            s"${value}.map(a => ${baseCoercion("a")})"
      }
      val coercedVariable =
        a.tpe match {
          case DataType.Color =>
            a.fieldTpe match {
              case DataType.Double => toResult({ v => s"XmlReader.rgbColorToHex(XmlReader.doubleToRgbColor(${v}))" })
              case _ => toResult({ v => s"XmlReader.rgbColorToHex(${v})" })
            }
          case DataType.RestrictionType(_, resultType, Restriction.Enum(options)) =>
            val typeName = resultType.toLowerCase.split('.').last
            val matchLines =
              s"${typeName} match {" +: options.map {
                case (stringValue, klassName) => s"""  case ${klassName} => "$stringValue""""
              } :+ "}"
              generator.coerce(s"${typeName}ToString", Seq((typeName, resultType)), "String", matchLines)
              // we want to do a case match here and map back to field name
              toResult({ v => s"${typeName}ToString(${v})" })
          case DataType.Points =>
            toResult({ v => s"""${v}.map(t => t._1 + "," + t._2).mkString(" ")"""})
          case _ => toResult({ v => s"${v}.toString" })
        }
      if (a.required)
        generator.write(s""".withAttribute("${a.name}", ${coercedVariable})""")
      else
        generator.write(s""".withOptionalAttribute("${a.name}", ${coercedVariable})""")
    }

    def generateComplexTypeWriteMatches(complexTypeSpecs: Seq[ComplexType])(implicit types: Map[String, ComplexType]): Seq[(String, String, String)] = {
      val allWidgets = complexTypeSpecs.filter(_.isTopType)
      val typeGroups = allWidgets.groupBy(_.klassName)
      typeGroups.flatMap {
        case (klassName, cts) =>
          if (cts.length == 1) {
            val ct = cts.head
            Seq((ct.name, ct.klassName, s"${ct.name.capitalize}Writer.write(${ct.name}, factory)"))
          } else {
            val allResolvedElements: Seq[(ComplexType, Seq[SpecifiedElement])] =
              cts.map(ct => (ct, ct.content.elements.elements.map(resolveElement(types))))
            val initAccum: Seq[Seq[(SpecifiedElement, ComplexType)]] = {
              val (ct, els) = allResolvedElements.head
              els.map(el => Seq((el, ct)))
            }
            val elements: Seq[Seq[(SpecifiedElement, ComplexType)]] =
              allResolvedElements.tail.foldLeft(initAccum) {
                case (acc, (ct, els)) =>
                  (acc zip els.map(el => (el, ct))).map {
                    case (accEl, el) => accEl :+ el
                  }
                }
            val differingElement = elements.find(_.map(es => es._1.tpe).distinct.length > 1).get
            val differingElementFieldName = differingElement.head._1.fieldName
            val differingElementSubcases =
              differingElement.map {
                case (el, ct) =>
                  (el.name, el.tpe.resolvedType, s"${ct.name.capitalize}Writer.write(${klassName.toLowerCase}, factory)")
              }

            Seq((klassName.toLowerCase, klassName,
              matchCase(s"${klassName.toLowerCase}.${differingElementFieldName}", differingElementSubcases, 6).mkString("\n")))
          }
      }.toSeq
    }

    def generateComplexTypeReader(types: Map[String, ComplexType])(spec: ComplexType): String = {
      implicit val tpes = types
      val generator = new ReaderGenerator(spec, 4)
      val aReaderDecls =
        spec.content.attributes.map(attr => generator.declare(s"${attr.name}Reader", generateAttributeReader(attr)))
      val aReaderAssignments =
        spec.content.attributes.map(attr => generator.assignment(s"${attr.name}Reader", Seq(attr.fieldName), attr.name))
      generateContentReader(spec.name, spec.content, generator)
      generator.buildReader
    }

    def resolveElement(types: Map[String, ComplexType])(elem: ElementDefinition): SpecifiedElement = {
      elem match {
        case s: SpecifiedElement => s
        case c: ComplexTypeReference =>
          val ct = types(c.refName)
          SpecifiedElement(ct.name, c.fieldName.getOrElse(ct.name), None, DataType.DeferredType(ct.name), c.min, c.max)
      }
    }

  def parseFile(source: File, elems: Seq[String], topTypes: Seq[String]): Seq[ComplexType] = {
    val root = XML.loadFile(source)
    val allAttributeGroups = root.child.collect {
      case e@Elem("xsd", "attributeGroup", _, _, _*) => parseAttributeGroup(e.asInstanceOf[Elem])
    }
    val attributeGroupMap = allAttributeGroups.map(g => g.name -> g).toMap
    val allWidgets = root.child.collect {
      case e@Elem("xsd", "complexType", Attribute("name", xml.Text(t), _), _, _*) if elems.contains(t) =>
        parseComplexType(e.asInstanceOf[Elem], attributeGroupMap, topTypes)
    }
    allWidgets
  }

  def parseAttributeGroup(attributeGroupSpec: Elem): AttributeGroup = {
    val name = attributeGroupSpec.attributes("name").text
    val childElems = attributeGroupSpec.child.collect {
      case e@Elem("xsd", "attribute", _, _, _*) => parseSpecifiedAttribute(e.asInstanceOf[Elem])
    }
    AttributeGroup(name, childElems)
  }

  def parseTypeAttribute(e: Node): Option[DataType] = {
    e.attributes.get("type").flatMap(_.headOption).map(_.text).map(DataType.fromName).map {t =>
      val boxed = e.appInfo("boxed").map(_.toBoolean).getOrElse(false)
      if (boxed) DataType.Boxed(t) else t
    }
  }

  def parseSpecifiedAttribute(attributeElem: Elem): SpecifiedAttribute = {
    val name = attributeElem.attributes("name").text
    val tpe = parseTypeAttribute(attributeElem).getOrElse(nestedAttributeType(attributeElem))
    val required = attributeElem.attribute("use").exists(_.text == "required")
    val fieldName = attributeElem.appInfo("fieldName").getOrElse(name)
    val fieldTpe = attributeElem.appInfo("fieldType").map(DataType.fromName).getOrElse(tpe)
    SpecifiedAttribute(name, fieldName, tpe, fieldTpe, required)
  }

  // we only handle two types of restrictions: Enumerations and string maxLength
  def nestedAttributeType(attributeElem: Elem): DataType = {
    val restriction = attributeElem \ "simpleType" \ "restriction"
    val base = (restriction \ "@base").headOption.map(_.text).map(DataType.fromName)
      .getOrElse(throw new Exception(s"Invalid attribute: $attributeElem"))
    val maxLength = (restriction \ "maxLength" \ "@value").headOption.map(_.text.toInt)
    if (base == DataType.String && maxLength.exists(_ == 1)) DataType.Character
    else { // not a character, assume enumeration
      val resultType = restriction.appInfo("className")
        .getOrElse(throw new Exception("enumeration type not allowed without className: " + attributeElem))
      val enums = (restriction \ "enumeration").map { e =>
        val stringValue = (e \ "@value").text
        val klassName = e.appInfo("className").getOrElse(stringValue)
        (stringValue, klassName)
      }.toSeq
      DataType.RestrictionType(base, resultType, Restriction.Enum(enums))
    }
  }

  def parseElementDefinition(elementElem: Node): ElementDefinition = {
    val minOccurs = (elementElem \ "@minOccurs").headOption.map(_.text.toInt).getOrElse(1)
    val maxOccurs = (elementElem \ "@maxOccurs").headOption.map(_.text)
      .map(m => if (m == "unbounded") None else Some(m.toInt))
      .getOrElse(Some(1))
    val fieldName = elementElem.appInfo("fieldName")
    (elementElem \ "@name").headOption.map(_.text).map { name =>
      val tpe = parseTypeAttribute(elementElem)
        .orElse((elementElem \ "complexType").headOption
          .map(e => parseComplexTypeContent(e, Map()))
          .map(DataType.NestedComplexType.apply _))
        .getOrElse(throw new Exception("Invalid element: " + elementElem))
      val additionalField = elementElem.appInfo("additionalField")
      SpecifiedElement(name, fieldName.getOrElse(name), additionalField, tpe, minOccurs, maxOccurs)
    }.orElse(
      (elementElem \ "@ref").headOption.map(_.text).map { refName =>
        ComplexTypeReference(refName, fieldName, minOccurs, maxOccurs)
      }
    ).getOrElse(throw new Exception("Invalid element: " + elementElem))
  }

  def parseSimpleContent(simpleContentElem: Node): Option[(ComplexTypeElements, Seq[SpecifiedAttribute])] = {
    val base = (simpleContentElem \ "extension" \ "@base").headOption.map(_.text).map(DataType.fromName)
    val fieldName =
      (simpleContentElem \ "extension").appInfo("fieldName").getOrElse("value")
    val attributes = (simpleContentElem \ "extension" \ "attribute")
      .map(e => parseSpecifiedAttribute(e.asInstanceOf[Elem]))
    base.map(b => (SimpleContent(b, fieldName), attributes))
  }

  def parseElement(sequenceElem: Node): Seq[ElementDefinition] = {
    (sequenceElem \ "element").map(parseElementDefinition).toSeq
  }

  def parseChoice(choiceElem: Node): Choice = {
    val choiceElements = choiceElem.flatMap(parseElement)
    val fieldName = choiceElem.appInfo("fieldName")
      .getOrElse(throw new Exception("Invalid choice specification: " + choiceElem + " should include a fieldName"))
    val klassName = choiceElem.appInfo("className")
    val passThrough = choiceElem.appInfo("passThrough").map(_.toBoolean).getOrElse(false)
    Choice(choiceElements, fieldName, klassName, passThrough)
  }

  def parseSequenceChild(sequenceElem: Node): Option[SequenceChild] = {
    (sequenceElem \ "choice").headOption.map(parseChoice) orElse
      (sequenceElem \ "element").headOption.map(parseElementDefinition)
  }

  def parseSequence(seqElem: Node): Option[Sequence] = {
    val variadic = seqElem.appInfo("variadic").map(_.toBoolean).getOrElse(false)
    val fieldName = seqElem.appInfo("fieldName")
    parseSequenceChild(seqElem).map(child => Sequence(child, fieldName, variadic))
  }

  def parseComplexType(ctElement: Node, sharedSpecs: Map[String, AttributeGroup], topTypes: Seq[String]): ComplexType = {
    val name = ctElement.attributes("name").text
    val isTopType = topTypes.contains(name)
    ComplexType(name, isTopType, parseComplexTypeContent(ctElement, sharedSpecs))
  }

  def parseComplexTypeContent(ctElement: Node, sharedSpecs: Map[String, AttributeGroup]): ComplexTypeContent = {
    val klassName = ctElement.appInfo("className")
      .orElse(Option(ctElement.attributes("name")).flatMap(_.headOption).map(_.text.capitalize))
      .getOrElse(throw new Exception("could not find className for: " + ctElement))
    val importKlassName = ctElement.appInfo("import").map(_.toBoolean).getOrElse(true)
    val additionalImports = ctElement.appInfo("additionalImports").map(_.split(" ").toSeq).getOrElse(Seq[String]())
    val isPassThrough = ctElement.appInfo("passThrough").map(_.toBoolean).getOrElse(false)
    val simpleContent = (ctElement \ "simpleContent").headOption.flatMap(parseSimpleContent)
    val imports = if (importKlassName && !isPassThrough) additionalImports :+ klassName else additionalImports
    if (simpleContent.isDefined) {
      (simpleContent.map {
        case (elems, attrs) =>
          ComplexTypeContent(attrs, elems, klassName, isPassThrough, imports)
      }).get
    } else {
      val allAttributes =
        ctElement.child.flatMap {
          case e@Elem("xsd", "attributeGroup", _, _, _*) =>
            sharedSpecs(e.attributes("ref").text).attributes
          case e@Elem("xsd", "attribute", _, _, _*) =>
            Seq(parseSpecifiedAttribute(e.asInstanceOf[Elem]))
          case _ => Seq()
        }
      val allElements = (ctElement \ "all").flatMap(parseElement)
      val choiceElement = (ctElement \ "choice").headOption.map(parseChoice)
      val sequenceElement = (ctElement \ "sequence").headOption.flatMap(parseSequence)
      val elements = choiceElement orElse sequenceElement getOrElse All(allElements)
      ComplexTypeContent(allAttributes, elements, klassName, isPassThrough, imports)
    }
  }
}
