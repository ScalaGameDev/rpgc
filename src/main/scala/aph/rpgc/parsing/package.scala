package aph.rpgc

import model._

import scala.util.parsing.combinator._

/** This package defines the machinery used for parsing characters defined by the classes in the
  * [[model]] package, using a human-readable DSL.  At present, no formal specification for this
  * DSL is available, but the documentation for individual parsers should have detailed examples.
  * The syntax is also fairly similar to the output produced by [[export.PlainText]] exporters.
  */
package object parsing {

  /** Trait for parsers specific to particular RPG systems.
    *
    * Subclasses need only implement the [[SystemParser#elementParser]] method.
    */
  trait SystemParser[Sys <: System] extends RegexParsers {
    /** Our DSL is whitespace-sensitive, so individual parsers need to handle it themselves. */
    override def skipWhitespace: Boolean = false

    /** Parse the given input into a list of elements.
      *
      * The input is assumed to use Unix terminators, and an additional newline is appended
      * before parsing, to make it irrelevant whether the input file ends with a newline or not.
      * In general, the parser should silently ignore trailing newlines in its input.
      */
    def parse(in: CharSequence): ParseResult[List[Element[Sys]]] =
      parseAll(rep(elementParser), in + "\n")

    /** Parser for an arbitrary element of this system. */
    protected def elementParser: Parser[Element[Sys]]

    /** Generic parser for "whitespace to end of line, and the following terminator". */
    protected def blankLine: Parser[String] = eol | """\s+\n""".r

    /** Generic parser for "the rest of this line, which is not all whitespace, with terminator". */
    protected def nonBlankLine: Parser[String] = """.*\S.*""".r <~ eol

    /** Generic parser for line terminators. */
    protected def eol: Parser[String] = "\n"
  }
}
