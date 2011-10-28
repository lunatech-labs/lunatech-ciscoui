package ldap.wrapper

import org.apache.directory.shared.ldap.model.entry.Entry

object RichEntryConversions {
  implicit def entry2richentry(entry :Entry) :RichEntry = RichEntry(entry)
}

object RichEntry {
  def apply(entry :Entry) = new RichEntry(entry)
}

class RichEntry(val entry :Entry) {
  def apply(attribute :String) = {
    this.entry.get(attribute).get().getValue().toString
  }
}