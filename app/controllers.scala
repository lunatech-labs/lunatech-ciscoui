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

  def index = Template("Application/index.xml", ('baseUrl -> configuration("application.baseUrl")))

  def search(operation: LdapConnection => Iterable[Entry]) :Result = {
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

  def search(query: String) :Result = {
    search((connection :LdapConnection) =>
      for (res <- connection.search(configuration("ldap.baseDn"), query, SearchScope.ONELEVEL, "*").asScala)
      yield res
    )
  }

  private def correct(number: String) = {
    val corrected = if(number.length > 3) "0" + number else number
    corrected.replaceAll("\\+", "00").replaceAll("\\s", "")
  }

  private def directoryEntry(name: String, attr: Attribute) = {
    if(attr == null || attr.get().isNull)
      null
    else
      <DirectoryEntry>
        <Name>{name}</Name>
        <Telephone>{
          val value = attr.get.getValue.toString;
          correct(value);
          }</Telephone>
      </DirectoryEntry>
  }

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

  def nameQuery(name: String) = name match {
    case "" => "(&(objectClass=person)(cn=*))"
    case _ => "(&(objectClass=person)(|(cn=%1$s*)(sn=%1$s*)(displayName=%1$s*)))".format(name)
  };

  def listAll = search(nameQuery(""))

  def entry(dn: String) = {
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
