package aph.rpgc
package parsing

import model._

/** Parser for system-agnostic objects.
  *
  * System-agnostic characters in the DSL look like this:
  *
  * {{{
  *   Character Name
  *   Block 1: Block 1, paragraph 1.  Blocks are denoted by a label, followed by a colon and a
  *     space, then the start of the text.  Continuation of the text to subsequent lines is
  *     denoted by indenting the text by at least two spaces.
  *
  *       Block 1, paragraph 2.  Paragraphs are separated by one or more blank lines.
  *     Inconsistent indentation is ignored (as long as all lines are indented at least 2 spaces).
  *   Block 2:
  *     The first paragraph in a block need not start on the same line as the block label.
  *
  *
  *   Block 3: Any extra blank lines at the end of a block (like those immediately above) are not
  *     preserved in the parsed character.
  * }}}
  *
  * Multiple characters can be included in one file.  The only requirement is that characters after
  * the first may not have colons in their names (so they are not parsed as extra text blocks of
  * the first character).
  */
object AgnosticParser extends SystemParser[Agnostic] {

  /** At present, the only element type is a character. */
  override def elementParser: Parser[Element[Agnostic]] = character

  /** A character is a name, followed by zero or more text blocks. */
  private def character: Parser[Character[Agnostic]] = name ~ rep(textBlock) <~ rep(blankLine) ^^ {
    case name ~ blocks => Character(name, blocks: _*)
  }

  /** A text block is a label, followed by zero or more paragraphs. */
  private def textBlock: Parser[TextBlock] = label ~ paragraphs ^^ {
    case label ~ paragraphs => TextBlock(label, paragraphs: _*)
  }

  /** The label for a text block is some text followed by a colon. */
  private def label: Parser[String] = rep(blankLine) ~> "[^:\n]+".r <~ ":"

  /** The paragraphs in a text block are separated by one or more blank lines. */
  private def paragraphs: Parser[List[String]] =
    leadParagraph ~ repsep(paragraph, rep1(blankLine)) ^^ {
      case None ~ paragraphs => paragraphs
      case Some(firstParagraph) ~ moreParagraphs => firstParagraph +: moreParagraphs
    }

  /** The first paragraph in a text block requires special handling.
    *
    * The result of this parser may be [[None]] if the block looks like this:
    * {{{
    *   Block:
    *
    *     The first paragraph in the block.
    * }}}
    */
  private def leadParagraph: Parser[Option[String]] =
    (opt(" " ~> ".+".r) <~ eol) ~ opt(paragraph) <~ rep(eol) ^^ {
      case None ~ maybeParagraph => maybeParagraph
      case maybeLine ~ None => maybeLine
      case Some(line) ~ Some(paragraph) => Some(line + "\n" + paragraph)
    }

  /** Parses a paragraph (possibly excepting the first line opening a text block). */
  private def paragraph: Parser[String] = rep1(indent ~> nonBlankLine) ^^ { _.mkString("\n") }

  /** Leading indentation must be at least two spaces. */
  private def indent: Parser[String] = "  +".r

  /** A name appears by itself on a line and starts with non-whitespace. */
  private def name: Parser[String] = """\S[^\n]*""".r <~ rep(eol)
}
