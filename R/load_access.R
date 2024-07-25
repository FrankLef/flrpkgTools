#' Load Data From MS Access
#'
#' Load data from an MS Access database.
#'
#' Extract the data from a local MS Access database using a list of queries.
#'
#' @param dir directory name of the MS Access db
#' @param db file name of the MS Access db
#' @param qries query names in the form "df_name" = "qry_name"
#' where df_name is the name that will be assigned to the data.frame
#' and qry_name is the query name in the db.
#' @param env The environment where the data will be located. Default is
#' \code{.GlobalEnv}. Don't change that unless you know why you are doing it.
#'
#' @importFrom RODBC odbcConnectAccess2007 odbcCloseAll sqlQuery
#'
#' @return nb of queries processed
#' @export
load_access <- function(dir = getwd(), db = NULL, qries = NULL, env = .GlobalEnv) {
  checkmate::assertDirectoryExists(dir)
  file <- file.path(dir, db)
  checkmate::assertFileExists(file, access = "r")
  checkmate::assertCharacter(qries, min.chars = 1, min.len = 1)


  # always close connections when exiting
  on.exit(RODBC::odbcCloseAll())

  tryCatch(
    {
      cnx <- RODBC::odbcConnectAccess2007(file, uid = "", pwd = "", readOnly = TRUE)
    },
    error = function(cond) {
      msg <- sprintf("Error connecting to %s\n%s", db, cond$message)
      logging::logerror(msg)
      stop(msg)
    },
    warning = function(cond) {
      msg <- sprintf("Warning connecting to %s\n%s", db, cond$message)
      logging::logwarn(msg)
      stop(msg)
    }
  )

  n <- 0
  for (i in seq_along(qries)) {
    msg <- sprintf("Extract %s", qries[i])
    logging::logdebug(msg)
    qry <- paste0("SELECT * FROM ", qries[i], ";")
    tryCatch(
      {
        df <- RODBC::sqlQuery(channel = cnx, query = qry, errors = TRUE)
      },
      error = function(cond) {
        msg <- sprintf("Error extracting query to \"%s\"\n%s", qry, cond$message)
        logging::logerror(msg)
        stop(msg)
      },
      warning = function(cond) {
        msg <- sprintf("Warning extracting query to \"%s\"\n%s", qry, cond$message)
        logging::logwarn(msg)
        stop(msg)
      }
    )
    if (!is.data.frame(df)) {
      msg <- sprintf("No data returned by \n\"%s\"", qry)
      logging::logerror(msg)
      stop(msg)
    }
    assign(x = names(qries)[i], value = df, envir = env)
    n <- n + 1
  }

  n
}
