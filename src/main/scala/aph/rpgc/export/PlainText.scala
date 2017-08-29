package aph.rpgc
package export

import model._

/** Object defining exporters for formatting model elements as plain text; i.e., with no markup at all. */
object PlainText extends Format {
  /** Partial application of format type, for concision. */
  type Exporter[Elem] = export.Exporter[Elem, this.type]

  implicit class TextBlockExporter(block: TextBlock) extends Exporter[TextBlock] {

    /** Prefix for the first line in the block. */
    private val label = block.label + ": "

    /** Prefix for subsequent lines in the block. */
    private val indent = "  "

    /** Line inserted between paragraphs. */
    private val spacer = ""

    private def exportParagraph(label: String, paragraph: String): Stream[String] = {
      val lines = paragraph.split("\n")
      (label + lines.head) +: lines.tail.map {indent + _}.toStream
    }

    override def export(): Stream[String] = {
      import block.paragraphs
      (exportParagraph(label, paragraphs.head)
        ++ paragraphs.tail.flatMap {spacer +: exportParagraph(indent, _)})
    }
  }

  implicit class CharacterExporter[Sys <: System](char: Character[Sys]) extends Exporter[Character[Sys]] {
    /** Line inserted before each block. */
    private val spacer = ""

    override def export(): Stream[String] = char.name +: char.blocks.flatMap {spacer +: _.export()}.toStream
  }
}
