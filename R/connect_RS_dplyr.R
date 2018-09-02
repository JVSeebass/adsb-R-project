#' Setup Connection for querying AWS Redshift with dplyr
#'
#' Function to establish PostgreSQL driven connection to AWS Redshift DBMS.
#' Communication with DB via this channel requires only dplyr verbs, no
#' PostgreSQL code is required.
#'
#' @param host A character string for the DB host
#' @param user A character string for the Username
#' @param dbname A character string for the Name of the DB
#' @param port Port is set to 5439 (class(port): "numeric") by default.
#' This is the default TCP port for AWS Redshift DBMS.
#'
#' @return Returns a "Formal Class PQConnection" object which can be
#' used to e.g. query the AWS Redshift DBMS with help of the
#' \code{\link[dplyr]{dplyr}}, more specifically the
#' \code{\link[dbplyr]{dbplyr}} package.
#'
#' @details Generally not executable examples:
#' Connecting to PostgreSQL AWS Redshift database hosted by
#' "redshift.adsbexchange.com" for user "erikseumegoettingen",
#' with database name "adsbx". Enter password in pop-up.
#'
#' con <- ConnectRSdplyr(host = 'redshift.adsbexchange.com',
#' user = 'erikseumegoettingen', dbname = 'adsbx')
#'
#' dbDisconnect(con)
#' dbIsValid(con) # confirms that connection is closed by returning FALSE
#'




# Writing a function that intakes few parameters, to create connection
# object to connect to a PostgreSQL database.

ConnectRSdplyr <- function(host, user, dbname, port = 5439) {

# Building the connection object by using DBI::dbConnect(driver, ...)
# where ... represents dates required to establish DB connection.

con <- DBI::dbConnect(drv  = RPostgres::Postgres(),
  host = host,
  port = port,
  user = user,
  # using pop up box to safely retrieve password
  password = rstudioapi::askForPassword("Database password"),
  dbname   = dbname,
  sslmode  = 'require')

}
