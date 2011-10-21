package controllers

import play._
import play.mvc._
import reflect.Template
import org.apache.directory.shared.ldap.model.message._
import org.apache.directory.ldap.client.api.{LdapConnection, LdapNetworkConnection}
import collection.JavaConverters._
import org.apache.directory.shared.ldap.model.cursor.EntryCursor
import org.apache.directory.shared.ldap.model.entry.Entry


object Application extends Controller {

  def index = Template("Application/index.xml", ('baseUrl -> configuration("application.baseUrl")))

  private val searchAll = {
    connection :LdapConnection =>
      for (res <- connection.search(configuration("ldap.baseDn"), "(&(objectclass=person)(cn=sietse*))", SearchScope.ONELEVEL, "*").asScala)
      yield res
  }

  def search = {
    val searchResults = searchWithLdapConnection(searchAll)
<CiscoIPPhoneMenu>
  <Title>Search results</Title>
  <Prompt>Found {searchResults.size} entries</Prompt>
  {
    for(result <- searchResults)
      yield
      <MenuItem>
        <Name>{result.get("displayName").get().getValue()}</Name>
        <URL>{configuration("application.baseUrl")}entries/{result.getDn}</URL>
    </MenuItem>
  }
</CiscoIPPhoneMenu>


  }

  def searchWithLdapConnection[T](operation: LdapConnection => T) :T  = {
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
