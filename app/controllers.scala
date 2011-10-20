package controllers

import play._
import play.mvc._
import reflect.Template
import org.apache.directory.shared.ldap.model.message._
import org.apache.directory.ldap.client.api.{LdapConnection, LdapNetworkConnection}
import collection.JavaConverters._
import org.apache.directory.shared.ldap.model.cursor.EntryCursor


object Application extends Controller {

  def index = Template("Application/index.xml", ('baseUrl -> configuration("application.baseUrl")))

  def search = {
    val res: EntryCursor = searchWithLdapConnection(conn => conn.search(configuration("ldap.baseDn"), "(objectclass=*)", SearchScope.ONELEVEL, "*"))

    for (result <- res.asScala)
      Logger.warn("%s", result)

    <foo/>
  }

  def searchWithLdapConnection(operation: LdapConnection => EntryCursor): EntryCursor = {
    val conn: LdapNetworkConnection = new LdapNetworkConnection(configuration("ldap.host"), configuration("ldap.port").toInt)
    try {
      conn.bind(configuration("ldap.user"), configuration("ldap.password"))
      val res: EntryCursor = conn.search(configuration("ldap.baseDn"), "(objectclass=*)", SearchScope.ONELEVEL, "*")
      res
    } finally {
      conn.unBind()
      conn.close()
    }
  }


}
