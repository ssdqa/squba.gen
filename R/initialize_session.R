

#' Initialize Argos Session for DQ work
#'
#' @param session_name arbitrary string to name the session
#' @param db_conn either a connection object used to connect to a relational database or the
#'                path to a json file with the relevant connection information
#' @param is_json a logical to indicate whether db_conn is the path to a json file or not
#' @param cdm_schema string name of the schema where the data in a CDM format is kept
#' @param results_schema string name of the schema where results should be output
#'
#' @return Argos session will be established in the global environment; this session is
#'         automatically established as the default
#'
#'         Function will print the database connection information and session
#'         information in the console for user review
#'
#' @import srcr
#' @importFrom DBI dbGetInfo
#' @export
#'
initialize_dq_session <- function(session_name,
                                  db_conn,
                                  is_json = FALSE,
                                  cdm_schema,
                                  results_schema,
                                  results_tag = NULL){

  # Establish session
  argos_session <- argos$new(session_name)

  set_argos_default(session = argos_session)

  # Set standard configs
  if(!is_json){
    get_argos_default()$config('db_src', db_conn)
  }else{
    get_argos_default()$config('db_src', srcr(db_conn))
  }

  get_argos_default()$config('cdm_schema', cdm_schema)
  get_argos_default()$config('results_schema', results_schema)
  get_argos_default()$config('results_name_tag', results_tag)

  # Print session information
  db_str <- DBI::dbGetInfo(config('db_src'))
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  cli::cli_inform(paste0('Connected to: ', db_str$dbname, '@', db_str$host))
  cli::cli_inform('To see environment settings, run {.code get_argos_default()}')

}
