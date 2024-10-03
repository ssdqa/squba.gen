

#' Initialize Argos Session for DQ work
#'
#' @param session_name arbitrary string to name the session
#' @param db_conn either a connection object used to connect to a relational database OR the
#'                path to a json file with the relevant connection information; if the latter,
#'                is_json should be set to TRUE
#' @param working_directory the base directory in which the analysis is taking place; defaults to the output of getwd()
#' @param file_subdirectory the *subdirectory* within the working directory where all files to be used in the analysis
#'                       (i.e. concept sets) are kept. this sets a default file location so the functions can easily
#'                       read in relevant files without having to redefine the path
#' @param is_json a logical to indicate whether db_conn is the path to a json file or not
#' @param cdm_schema string name of the schema where the data in a CDM format is kept
#' @param results_schema string name of the schema where results should be output if the user chooses
#'                       to utilize the `results_tbl` function native to the argos environment;
#'                       defaults to NULL
#' @param results_tag string to indicate a suffix (if any) that should be appended onto
#'                    any results tables; defaults to NULL
#' @param vocabulary_schema string name of the schema where vocabulary tables (i.e. concept) are
#'                          stored on the database
#'
#' @return Argos session will be established in the environment; this session is
#'         automatically established as the default that will appear when `get_argos_default`
#'         is called
#'
#'         Function will print the database connection information and session
#'         information in the console for user review
#'
#' @import srcr
#' @importFrom DBI dbGetInfo
#' @importFrom stringr str_remove
#' @export
#'
initialize_dq_session <- function(session_name,
                                  db_conn,
                                  working_directory = getwd(),
                                  file_subdirectory,
                                  is_json = FALSE,
                                  cdm_schema,
                                  results_schema = NULL,
                                  vocabulary_schema = NULL,
                                  results_tag = NULL){

  # Establish session
  argos_session <- argos$new(session_name)

  set_argos_default(argos_session)

  #assign(session_name, argos_session, envir = .GlobalEnv)

  # Set standard configs
  if(!is_json){
    get_argos_default()$config('db_src', db_conn)
  }else{
    get_argos_default()$config('db_src', srcr(db_conn))
  }

  get_argos_default()$config('cdm_schema', cdm_schema)
  get_argos_default()$config('results_schema', results_schema)
  get_argos_default()$config('vocabulary_schema', vocabulary_schema)
  get_argos_default()$config('cache_enabled', FALSE)
  get_argos_default()$config('retain_intermediates', FALSE)
  get_argos_default()$config('db_trace', TRUE)
  get_argos_default()$config('can_explain',
                             !is.na(tryCatch(db_explain(config('db_src'), 'select 1 = 1'),
                                             error = function(e) NA)))

  if(is.null(results_tag)){
    get_argos_default()$config('results_name_tag', '')
  }else{
    get_argos_default()$config('results_name_tag', results_tag)
  }

  # Set working directory
  get_argos_default()$config('base_dir', working_directory)

  # Set specs directory
  ## Drop path to working directory if present
  drop_wd <- str_remove(file_subdirectory, working_directory)
  get_argos_default()$config('subdirs', list(spec_dir = drop_wd))

  # Print session information
  db_str <- DBI::dbGetInfo(config('db_src'))
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  cli::cli_inform(paste0('Connected to: ', db_str$dbname, '@', db_str$host))
  cli::cli_inform('To see environment settings, run {.code get_argos_default()}')

}
