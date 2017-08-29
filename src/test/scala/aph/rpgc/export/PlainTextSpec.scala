package aph.rpgc
package export

import model.{Agnostic, Character, TextBlock}
import PlainText._

import org.scalatest.FunSuite

class PlainTextSpec extends FunSuite {

  object Alice {
    val name = "Alice"

    val char = new Character[Agnostic]("Alice",
      TextBlock("Description",
        """Description paragraph 1, line 1.
          |Description paragraph 1, line 2.""".stripMargin,
        "Description paragraph 2, line 1."
      ),
      TextBlock("Background",
        """Background paragraph 1, line 1.
          |Background paragraph 1, line 2.""".stripMargin
      )
    )

    val expected: String =
      """Alice
        |
        |Description: Description paragraph 1, line 1.
        |  Description paragraph 1, line 2.
        |
        |  Description paragraph 2, line 1.
        |
        |Background: Background paragraph 1, line 1.
        |  Background paragraph 1, line 2.""".stripMargin
  }

  test("Alice exports correctly") {
    assert(Alice.char.export().mkString("\n") == Alice.expected)
  }
}
