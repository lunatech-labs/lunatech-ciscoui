package ldap.wrapper

import org.apache.directory.shared.ldap.model.entry.{Value, Attribute, Entry}

object RichEntryConversions {
  implicit def entry2richentry(entry :Entry) :RichEntry = RichEntry(entry)
}

object RichEntry {
  def apply(entry :Entry) = new RichEntry(entry)
}

class RichEntry(val entry :Entry) {
  def apply(attribute :String) :Option[String] = {
    for {
      a <- Option(this.entry.get(attribute))
      v <- Option(a.get())
    } return Some(v.toString)
    None
  }
}