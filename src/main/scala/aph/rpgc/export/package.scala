package aph.rpgc

/** This package defines the machinery used for exporting characters defined by the classes in
  * the [[model]] package, as well as generic exporters for the concrete classes defined there.
  *
  * The [[export.Exporter]] trait is a typeclass enriching elements of a parametrized type with
  * knowledge of how to export themselves in a particular format, such as plain text, LaTeX, or
  * HTML.  Each format is represented by an object extending the [[export.Format]] trait and
  * containing [[export.Exporter]] implementations for each of the system-agnostic classes
  * defined in the [[model]] package.
  */
package object export {

  /** Marker trait for export formats. */
  trait Format

  /** Typeclass enriching an element of type [[Elem]] with an export method.
    *
    * Typically [[Elem]] will be an instance of [[model.Element]], but this is not enforced, so
    * that non-top-level system objects (e.g., [[model.TextBlock]]) can be exported.
    */
  trait Exporter[Elem, F <: Format] {
    /** Converts the enriched element to a [[Stream]] of lines. */
    def export(): Stream[String]
  }
}
