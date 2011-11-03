package ldap.wrapper

import org.apache.directory.shared.ldap.model.entry.{Value, Attribute, Entry}

/**Contains implicits for RichEntry
 */
object RichEntryConversions {
  /**Converts an Entry to RichEntry when needed
   */
  implicit def entry2richentry(entry :Entry) :RichEntry = RichEntry(entry)
}

/**Companion Object for RichEntry
 */
object RichEntry {
  /**Constructs a new RichEntry wrapper for an Entry
   */
  def apply(entry :Entry) = new RichEntry(entry)
}

/** Wrapper class to make Entries more Scala-like
 */
class RichEntry(val entry :Entry) {
  /** Returns the string value for an attribute on this entry
   * @param attribute The name of the attribute for which to return the value
   */
  def apply(attribute :String) :Option[String] = {
    for {
      a <- Option(this.entry.get(attribute))
      v <- Option(a.get())
    } return Some(v.toString)
    None
  }
}