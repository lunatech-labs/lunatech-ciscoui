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
import ldap.wrapper.RichEntryConversions._
import results.{NotFound, Result}
import xml.NodeSeq

object Application extends Controller {

  /**Renders the Cisco IP Phone main menu.
   *
   */
  def index = Xml(<CiscoIPPhoneMenu><Title>Services</Title>
    <Prompt>Choose</Prompt>
    <MenuItem>
        <Name>Find by name</Name>
        <URL>{configuration("application.baseUrl")  + Router.reverse("Application.searchByName")}</URL>
    </MenuItem>
    <MenuItem>
        <Name>Find by telephone number</Name>
        <URL>{configuration("application.baseUrl")  + Router.reverse("Application.searchByNumber")}</URL>
    </MenuItem>
    <MenuItem>
        <Name>List Lunatech employees</Name>
        <URL>{configuration("application.baseUrl")  + Router.reverse("Application.searchByEmail", Map[String, Object]("address" -> "lunatech.com").asJava)}</URL>
    </MenuItem>
</CiscoIPPhoneMenu>)

  /**Lists all people in the LDAP directory as a Cisco IP Phone menu.
   * Equivalent to searchByName("")
   */
  def listAll = search(nameQuery(""))

  /** Search LDAP using the given query string.
   * @param query The query to use for the search.
   */
  def search(query: String) :Result = {
    search { (connection :LdapConnection) =>
        for (res <- connection.search(configuration("ldap.baseDn"), query, SearchScope.SUBTREE, "*").asScala)
        yield res
    }
  }

  /**Searches the LDAP directory for people by name, and renders a Cisco IP Phone menu with the results.
   * If no name is given, prompts for input.
   * @param name The (first part of the) name to search on.
   */
  def searchByName(name: Option[String]) = {
    name match {
      case None => Xml(
        <CiscoIPPhoneInput>
          <Title>Find by name</Title>
          <Prompt>Enter (part of) a name</Prompt>
          <URL>{configuration("application.baseUrl")  + Router.reverse("Application.searchByName")}</URL>
          <InputItem>
            <DisplayName>Name</DisplayName>
            <QueryStringParam>name</QueryStringParam>
            <DefaultValue></DefaultValue>
            <InputFlags>L</InputFlags>
          </InputItem>
        </CiscoIPPhoneInput>)
      case Some(n) => search(nameQuery(n))
    }
  }

  /**Searches the LDAP directory for people by phone number, and renders a Cisco IP Phone menu with the results.
   * If no number is given, prompts for input.
   * @param number The number to search on.
   */
  def searchByNumber(number: Option[String]) = {
    number match {
      case None => Xml(
        <CiscoIPPhoneInput>
          <Title>Find by number</Title>
          <Prompt>Enter a number</Prompt>
          <URL>{configuration("application.baseUrl")  + Router.reverse("Application.searchByNumber")}</URL>
          <InputItem>
            <DisplayName>Phone number</DisplayName>
            <QueryStringParam>phone</QueryStringParam>
            <DefaultValue></DefaultValue>
            <InputFlags>T</InputFlags>
          </InputItem>
        </CiscoIPPhoneInput>)
      case Some(n) => search(numberQuery(n))
    }
  }

  /**Searches the LDAP directory for people by email address, and renders a Cisco IP Phone menu with the results.
   * If no address is given, prompts for input.
   * @param name (Part of) the e-mail address to search on.
   */
  def searchByEmail(address: Option[String]) = {
    address match {
      case None => Xml(
        <CiscoIPPhoneInput>
          <Title>Find by e-mail address</Title>
          <Prompt>Enter (part of) an address</Prompt>
          <URL>{configuration("application.baseUrl")  + Router.reverse("Application.searchByEmail")}</URL>
          <InputItem>
            <DisplayName>E-mail address</DisplayName>
            <QueryStringParam>address</QueryStringParam>
            <DefaultValue></DefaultValue>
            <InputFlags>L</InputFlags>
          </InputItem>
        </CiscoIPPhoneInput>)
      case Some(a) => search(emailQuery(a))
    }
  }

  /**
   * Looks up the object for the given DN in LDAP, and renders it as a Cisco Phone Directory entry.
   * @param dn The DN for the object to render.
   */
  def entry(dn: String) =  {
    val lookup = (connection: LdapConnection) => Option(connection.lookup(dn))
    val entry = withLdapConnection(lookup)

    entry match {
      case None => NotFound
      case Some(entry) => Xml(
        <CiscoIPPhoneDirectory>
          <Title>{entry("displayName").getOrElse("")}</Title>
          <Prompt>Choose an entry</Prompt>
          {directoryEntry("Main", entry("telephoneNumber"))}
          {directoryEntry("Office", entry("telephoneNumberAlternate"))}
          {directoryEntry("Home", entry("homePhone"))}
          {directoryEntry("Mobile", entry("mobile"))}
        </CiscoIPPhoneDirectory>)
    }
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
          <Name>{result("displayName")}</Name>
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
  private def directoryEntry(label: String, value: Option[String]) = {
    value match {
      case Some(v) =>
        <DirectoryEntry>
          <Name>{label}</Name>
          <Telephone>{
            correct(v);
            }</Telephone>
        </DirectoryEntry>
      case None => NodeSeq.Empty
    }
  }

  /** Returns an LDAP search filter that filters by a person's name, or returns all entries in the address book.
   * @param name The (first part of the) name to search on.
   */
  private def nameQuery(name: String) = name match {
    case "" => "(&(objectClass=person)(cn=*))"
    case _ => "(&(objectClass=person)(|(cn=%1$s*)(sn=%1$s*)(displayName=%1$s*)))".format(name)
  };

  /** Returns an LDAP search filter that filters on phone number, or returns all entries in the address book.
   * @param name The (first part of the) number to search on.
   */
  private def numberQuery(number: String) = number match {
    case "" => "(&(objectClass=person)(|(telephoneNumber=*)(homePhone=*)(mobile=*)))"
    case _ => "(&(objectClass=person)(|(telephoneNumber=*%1$s*)(pager=*%1$s*)(homePhone=*%1$s*)(mobile=*%1$s*)))".format(number)
  };

  /** Returns an LDAP search filter that filters on phone number, or returns all entries in the address book.
   * @param name The (first part of the) number to search on.
   */
  private def emailQuery(address: String) = address match {
    case "" => "(&(objectClass=person)(cn=*))"
    case _ => "(&(objectClass=person)(mail=*%s*))".format(address)
  };

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
