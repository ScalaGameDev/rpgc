package aph.rpgc
package parsing

import model._
import AgnosticParser.{NoSuccess, Success}

import org.scalatest.FunSuite

class AgnosticParserSpec extends FunSuite {

  def checkParsing(input: String, expected: List[Character[Agnostic]]): Unit = {
    AgnosticParser.parse(input) match {
      case NoSuccess(msg, _) => fail(s"Parsing error: $msg")
      case Success(chars, _) => assert(chars == expected)
    }
  }

  trait TestCharacter {
    def name: String
    def input: String
    def expected: Character[Agnostic]

    def check(): Unit = test(s"$name parses correctly") { checkParsing(input, List(expected)) }
  }

  object Alice extends TestCharacter {
    val name = "Alice"
    val input = "Alice"
    val expected = Character(name)
  }
  Alice.check()

  object Bob extends TestCharacter {
    val name = "Bob"
    val input: String =
      """Bob
        |Description: Bob has a description.
      """.stripMargin
    val expected = Character("Bob",
      TextBlock("Description", "Bob has a description."))
  }
  Bob.check()

  object Charlie extends TestCharacter {
    val name = "Charlie"
    val input: String =
      """Charlie
        |
        |Description: Charlie has a blank line after his name.
      """.stripMargin
    val expected = Character("Charlie",
      TextBlock("Description", "Charlie has a blank line after his name."))
  }
  Charlie.check()

  object Deirdre extends TestCharacter {
    val name = "Deirdre"
    val input: String =
      """Deirdre
        |Description: Deirdre has a
        |  multiline description.""".stripMargin
    val expected = Character("Deirdre",
      TextBlock("Description",
      """Deirdre has a
        |multiline description.""".stripMargin))
  }
  Deirdre.check()

  object Evan extends TestCharacter {
    val name = "Evan"
    val input: String = (
      """Evan
        |Description: Evan has a
        |  multiple-paragraph description.
        |
        |  This is the second paragraph.
      """.stripMargin
        + "  \n"
        + "  This is the third paragraph; the blank line immediately preceding has spaces in it."
      )
    val expected = Character("Evan",
      TextBlock("Description",
      """Evan has a
        |multiple-paragraph description.""".stripMargin,
      "This is the second paragraph.",
      "This is the third paragraph; the blank line immediately preceding has spaces in it."))
  }
  Evan.check()

  object Fiona extends TestCharacter {
    val name = "Fiona"
    val input: String =
      """Fiona
        |Description: Fiona has multiple
        |  text blocks.
        |Background: This is the second block.""".stripMargin
    val expected = Character("Fiona",
      TextBlock("Description",
      """Fiona has multiple
        |text blocks.""".stripMargin),
      TextBlock("Background",
      "This is the second block."))
  }
  Fiona.check()

  object George extends TestCharacter {
    val name = "George"
    val input: String =
      """George
        |Description: George has both multiple
        |  text blocks and multiple paragraphs.
        |
        |  This is the second description paragraph.
        |
        |Background: This is the first background paragraph.
        |  Notice the blank line preceding this block.
        |
        |  This is the second background paragraph.
        |  So is this.
        |
        |  This is the third background paragraph.""".stripMargin
    val expected = Character("George",
      TextBlock("Description",
      """George has both multiple
        |text blocks and multiple paragraphs.""".stripMargin,
      "This is the second description paragraph."),
      TextBlock("Background",
        """This is the first background paragraph.
          |Notice the blank line preceding this block.""".stripMargin,
      """This is the second background paragraph.
        |So is this.""".stripMargin,
      "This is the third background paragraph.")
    )
  }
  George.check()

  object Henry extends TestCharacter {
    val name = "Henry"
    val input: String =
      """Henry
        |Description: Henry and Henriette are twins.""".stripMargin
    val expected: Character[Agnostic] = Character("Henry",
      TextBlock("Description", "Henry and Henriette are twins."))
  }

  object Henriette extends TestCharacter {
    val name = "Henriette"
    val input: String =
      """Henriette
        |Description: Henry and Henriette are twins,
        |  but Henriette has more description.""".stripMargin
    val expected: Character[Agnostic] = Character("Henriette",
      TextBlock("Description",
        """Henry and Henriette are twins,
          |but Henriette has more description.""".stripMargin)
    )
  }

  test("Henry and Henriette parse correctly") {
    checkParsing(
      input = Henry.input + "\n\n" + Henriette.input,
      expected = List[Character[Agnostic]](Henry.expected, Henriette.expected)
    )
  }

  object Ichabod extends TestCharacter {
    val name = "Ichabod"
    val input = "Ichabod: The Destroyer"
    val expected = Character("Ichabod: The Destroyer")
    val malformedBlock = TextBlock("Ichabod", "The Destroyer")
  }
  Ichabod.check()

  test("Ichabod parses incorrectly when not first") {
    checkParsing(
      input = Henry.input + "\n\n" + Ichabod.input,
      expected = List(Character(Henry.expected.name,
        Henry.expected.blocks :+ Ichabod.malformedBlock: _*))
    )
  }

  object Juliet extends TestCharacter {
    val name = "Juliet"
    val input: String =
      """Juliet
        |Description:
        |  Juliet starts her blocks on the line after the label.
        |
        |Background:
        |  Here's Juliet's background block.
        |
        |  Here's the second paragraph of Juliet's background.
      """.stripMargin
    val expected = Character("Juliet",
      TextBlock("Description", "Juliet starts her blocks on the line after the label."),
      TextBlock("Background",
        "Here's Juliet's background block.",
        "Here's the second paragraph of Juliet's background."))
  }
  Juliet.check()

  object Kenny extends TestCharacter {
    val name = "Kenny"
    val input: String =
      """Kenny
        |Description:
        |    Kenny uses four spaces for indentation.
        |
        |    Here's a second paragraph.""".stripMargin
    val expected = Character("Kenny",
      TextBlock("Description",
      "Kenny uses four spaces for indentation.",
      "Here's a second paragraph."))
  }
  Kenny.check()
}