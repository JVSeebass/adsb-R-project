#' Connect to AWS Redshift DBMS using JDBC Driver
#'
#' Function to establish JDBC driven connection to AWS Redshift DBMS.
#' Credentials and cluster information required. Communication with
#' Redshift DB, which runs on a modified PostgreSQL dialect, can
#' require special
#' \href{https://docs.aws.amazon.com/redshift/latest/dg/c_SQL_commands.html}{Redshift PostgreSQL code}.
#'
#'
#' @details Either provide: yrurl OR all other arguments.
#' Connection to DB not visible in RStudio connection pane. Disconnect
#' by using e.g. \link[DBI]{dbDisconnect}
#'
#' @param yrurl A character string of the URL to the cluster of the DBMS.
#' Paste only the following part of the URL to function input:
#' yrurl = "<JDBCURL>:<port>/<dbname>?user=<user>&". If yrurl is supplied,
#' ConnectRS requires no further arguments as inputs.
#'
#' @param host A character string of the host
#'
#' @param dbname A character string of the DB name
#'
#' @param user A character string of the username
#'
#' @param driverClass A character string of with the Class of the JDBC Driver.
#' Check
#' \href{https://docs.aws.amazon.com/redshift/latest/mgmt/configure-jdbc-connection.html#obtain-jdbc-url}{AWS Documentation}
#' for up to date driver class and adjust following link if necessary
#' "com.amazon.redshift.jdbc42.Driver".
#' If new driver class 43 is available enter e.g.:
#' "com.amazon.redshift.jdbc43.Driver"
#'
#' @param classPath A character string of the path to the folder containing
#' the JDBC-Driver (a .jar file)
#'
#' @param identifier.quote Optional argument; NA by default; can impact
#' functions as e.g.
#' \code{\link[DBI]{dbWriteTable}} or \code{\link[DBI]{dbReadTable}}
#' if not chosen correctly; see also \code{\link[RJDBC]{JDBC}} or
#' \href{https://docs.aws.amazon.com/redshift/latest/dg/r_names.html}{AWS Documentation}
#'
#' @examples \dontrun{
#' Not generally executable examples:
#'
#' con <- ConnectRS(yrurl = "jdbc:redshift://redshift.adsbexchange.com:5439/adsbx?user=random.user&")
#' (when the url is given)
#'
#' where:
#' <JDBCURL> <- jdbc:redshift://redshift.adsbexchange.com
#' <port> <- 5439
#' <dbname> <- adsbx
#' user=<user> <- random.user
#' Password retrieved via popo-up
#'
#' dbDisconnect(con)
#'
#' con <- ConnectRS(host = "redshift.adsbexchange.com", dbname = "adsbx",
#' user = "random.user", driverClass = "com.amazon.redshift.jdbc42.Driver",
#' classPath = "C:/.../JDBC/RedshiftJDBC42-1.2.15.1025.jar", identifier.quote = "`")
#' Password retrieved via popo-up
#'
#' dbDisconnect(con)}
#'
#' @details For details on the JDBC cluster URL see:
#' \href{https://docs.aws.amazon.com/redshift/latest/mgmt/configure-jdbc-connection.html#obtain-jdbc-url}{AWS Documentation}
#'
#' @return Returns a connection object which can be used to query
#' the AWS Redshift DBMS with e.g.: \code{\link[DBI]{dbGetQuery}}.
#'
#' @import DBI
#' @importFrom RJDBC JDBC
#'
#' @export ConnectRS



# Writing a funtion, which intakes few parameters, to create connection
# object to connect to a database with a JDBC driver. Driver information
# is always required

ConnectRS <- function(yrurl = NULL, host = NULL, dbname = NULL,
                      user = NULL, driverClass  = NULL,
                      classPath = NULL, identifier.quote = NULL) {

  # Define logical vectors to deteremine in 1) and 2), if necessary inputs
  # were provided by the user.

  a <- c(is.null(host), is.null(dbname), is.null(user), is.null(driverClass),
         is.null(classPath), is.null(identifier.quote))

  b <- c(is.null(yrurl), is.null(driverClass), is.null(classPath),
         is.null(identifier.quote))

  # 1)
  # All inputs except yrurl were provided.
  # The connnection object is created and returned for further use.

 if (all(a == FALSE)) {

  # Initializing JDBC driver (after checking if useable inputs are provided)
  driverClass      <- driverClass
  classPath        <- classPath
  identifier.quote <- identifier.quote

  driver           <- RJDBC::JDBC(
                        driverClass = driverClass,
                        classPath   = classPath,
                        identifier.quote = identifier.quote)


  JDBCURLprefix <- "jdbc:redshift://"
  host          <- host
  dbname        <- dbname
  port          <- ":5439/"  # Redshift default TCP port number
  user          <- user
  password      <- rstudioapi::askForPassword("Database password")

  # URL is constructed
  url           <- paste(JDBCURLprefix, host, port, dbname, "?",
                         "user=", user, "&", "password=", password, sep = "")
  # url and driver are used to establish connection
  con           <- DBI::dbConnect(driver, url)

  return(con)


  # 2)
  # yrurl is provided as well as the information for the driver.
  # That is the driverClass, classPath and identifier.quote

  } else if (all(b == FALSE)) {

  # Initializing JDBC driver (after checking if useable inputs are provided)
  driverClass      <- driverClass
  classPath        <- classPath
  identifier.quote <- identifier.quote

  driver           <- RJDBC::JDBC(
                        driverClass = driverClass,
                        classPath   = classPath,
                        identifier.quote = identifier.quote)

  password <- rstudioapi::askForPassword("Database password")
  yyrurl   <- paste(yrurl, "&","password=", password, sep = "")

  # url and driver are used to establish connection
  con      <- DBI::dbConnect(driver, yyrurl)

  return(con)


  # 3) Inputs for cases 1) and 2) were not provided
  } else {
    stop("specify arguments correctly. see documentation for help")
  }
}

