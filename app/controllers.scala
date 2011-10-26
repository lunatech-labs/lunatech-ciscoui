package controllers

import play._
import play.mvc._
import reflect.Template
import org.apache.directory.shared.ldap.model.message._
import org.apache.directory.ldap.client.api.{LdapConnection, LdapNetworkConnection}
import collection.JavaConverters._
import org.apache.directory.shared.ldap.model.cursor.EntryCursor
import java.net.URLEncoder
import org.apache.directory.shared.ldap.model.entry.{Attribute, Entry}
import results.{Result, RenderXml}

object Application extends Controller {

  /**Renders the Cisco IP Phone main menu.
   *
   */
  def index = Template("Application/index.xml", ('baseUrl -> configuration("application.baseUrl")))

  /**Lists all people in the LDAP directory as a Cisco IP Phone menu.
   * Equivalent to searchByName("")
   */
  def listAll = search(nameQuery(""))

  /** Search LDAP using the given query string.
   * @param query The query to use for the search.
   */
  def search(query: String) :Result = {
    search((connection :LdapConnection) =>
    for (res <- connection.search(configuration("ldap.baseDn"), query, SearchScope.ONELEVEL, "*").asScala)
    yield res
    )
  }

  /**Searches the LDAP directory for people by name, and renders a Cisco IP Phone menu.
   * If no name is given, prompts for input.
   * @param name The (first part of the) name to search on.
   */
  def searchByName(name: String) = {
    if(name == null) Xml(
      <CiscoIPPhoneInput>
        <Title>Find by name</Title>
        <Prompt>Enter (part of) a name</Prompt>
        <URL>{Router.reverse("Application.searchByName")}</URL>
        <InputItem>
          <DisplayName>Name</DisplayName>
          <QueryStringParam>name</QueryStringParam>
          <DefaultValue></DefaultValue>
          <InputFlags>L</InputFlags>
        </InputItem>
      </CiscoIPPhoneInput>)
    else search(nameQuery(name))
  }

  /** Corrects a phonenumber such that a Cisco phone can dial it (e.g. 00031107502600).
   * @param number The number to correct.
   */
  private def correct(number: String) = {
    val corrected = if(number.length > 3) "0" + number else number
    corrected.replaceAll("\\+", "00").replaceAll("\\s", "")
  }

  /** Executes an LDAP search and renders a Cisco IP Phone Menu using the results.
   * @param operation The function doing the actual search
   */
  private def search(operation: LdapConnection => Iterable[Entry]) :Result = {
    val searchResults = withLdapConnection(operation)
    Xml(<CiscoIPPhoneMenu>
      <Title>Search results</Title>
      <Prompt>Found {searchResults.size} entries</Prompt>
      {
      for(result <- searchResults)
    yield
      <MenuItem>
          <Name>{result.get("displayName").get().getValue()}</Name>
          <URL>{configuration("application.baseUrl")}/entries/{URLEncoder.encode(result.getDn.toString, "UTF-8")}</URL>
        </MenuItem>
      }
    </CiscoIPPhoneMenu>)
  }

  /**Renders a Cisco IP Phone Directory Entry.
   *
   * @param label The "name" label for this entry.
   * @param attr The LDAP attribute for which to render this entry.
   */
  private def directoryEntry(label: String, attr: Attribute) = {
    if(attr == null || attr.get().isNull)
    null
    else
    <DirectoryEntry>
        <Name>{label}</Name>
        <Telephone>{
          val value = attr.get.getValue.toString;
          correct(value);
          }</Telephone>
      </DirectoryEntry>
  }

  /** Executes an LDAP search by a person's name, or returns all entries in the address book.
   * @param name The (first part of the) name to search on.
   */
  private def nameQuery(name: String) = name match {
    case "" => "(&(objectClass=person)(cn=*))"
    case _ => "(&(objectClass=person)(|(cn=%1$s*)(sn=%1$s*)(displayName=%1$s*)))".format(name)
  };


  /**
   * Looks up the object for the given DN in LDAP, and renders it as a Cisco Phone Directory entry.
   * @param dn The DN for the object to render.
   */
  private def entry(dn: String) = {
    val lookup = (connection: LdapConnection) => connection.lookup(dn)
    val entry = withLdapConnection(lookup)

    Xml(<CiscoIPPhoneDirectory>
      <Title>{entry.get("displayName").get().getValue}</Title>
      <Prompt>Choose an entry</Prompt>
      {directoryEntry("Main", entry.get("telephoneNumber"))}
      {directoryEntry("Office", entry.get("telephoneNumberAlternate"))}
      {directoryEntry("Home", entry.get("homePhone"))}
      {directoryEntry("Mobile", entry.get("mobile"))}
    </CiscoIPPhoneDirectory>)
  }

  /** Executes an operation on a new LDAP connection, then closes the connection.
   * @param operation The operation to perform.
   */
  private def withLdapConnection[T](operation: LdapConnection => T) :T  = {
    val connection: LdapNetworkConnection = new LdapNetworkConnection(configuration("ldap.host"), configuration("ldap.port").toInt)
    try {
      connection.bind(configuration("ldap.user"), configuration("ldap.password"))
      operation(connection)
    } finally {
      connection.unBind()
      connection.close()
    }
  }

}
