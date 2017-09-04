package aph.rpgc

/** This package defines the abstract model RPGC uses for RPG system data; i.e., characters and other rules elements
  * under compilation.  In addition, it defines concrete classes for object types common across RPG systems.  These
  * can be used to compile system-agnostic characters, or as base classes for model objects for specific systems.
  */
package object model {

  /** Marker trait used for identifying RPG systems. */
  trait System

  /** System for objects that are effectively system-independent. */
  trait Agnostic extends System

  /** Generic object representing a labelled chunk of text.
    *
    * This is a component of an [[Element]], not an independent entity, and so it neither is an [[Element]] nor is
    * associated with a [[System]].*/
  final case class TextBlock(
    /** The label for this block. */
    label: String,

    /** The text for this block, as a list of paragraphs.
      *
      * Paragraphs are separated here for the convenience of exporters.  Most export formats have a notion of
      * paragraph, and keeping the paragraphs cleanly separated should make it easier to maintain proper formatting. */
    paragraphs: String*
  ) {
    require(label != null)
    require(paragraphs != null)
  }

  /** Root class for all model objects in a given [[System]]. */
  abstract class Element[Sys <: System](
    /** The element's name inside its RPG system.
      *
      * If a bit of RPG "stuff" doesn't have a name, then it's probably not a top-level element and shouldn't
      * implement this trait.  Once validation is implemented, there will probably need to be some kind of database of
      * elements to be validated against, and this field will be a natural index for that database. */
    val name: String,

    /** Additional text, in the form of labelled [[TextBlock]]s. */
    val blocks: TextBlock*
  ) {
    require(name != null)
    require(blocks != null)
  }

  /** Represents the minimal definition of an RPG character. */
  class Character[Sys <: System](
    name: String,
    blocks: TextBlock*
  ) extends Element[Sys](name, blocks: _*)
}
