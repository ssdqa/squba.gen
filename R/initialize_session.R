
#' Initialize argos Session for squba Analysis
#'
#' This is a wrapper function that will create an argos session and set up
#' internal configurations that will allow all downstream functions to access
#' argos convenience functions. This step is **REQUIRED**, and makes it
#' easier to connect to a backend database to conduct analyses. The standard
#' argos workflow can also be used, but this wrapper is provided for convenience.
#'
#' @param session_name *string* || **required**
#'
#'   An arbitrary name to identify the session
#'
#' @param db_conn *string* / *database connection* || **required**
#'
#'   Either a connection object used to connect to a relational database
#'   (ex: the output of [DBI::dbConnect()]) OR a string indicating the path to
#'   a JSON file with relevant connection information
#'
#'   This information will be used to connect to the CDM and access the data
#'   indicated by the user for each analysis
#'
#' @param is_json *boolean* || defaults to `FALSE`
#'
#'   A boolean indicating whether `db_conn` is a database connection object (`FALSE`)
#'   or the path to a JSON file with connection details (`TRUE`)
#'
#' @param working_directory *string* || defaults to the result of [base::getwd()]
#'
#'   The base directory in which the analysis is taking place
#'
#'   It is expected that both the file & results subdirectories are directories
#'   downstream of this base directory.
#'
#' @param file_subdirectory *string* || **required**
#'
#'   The *subdirectory* within the working_directory where all files to be used
#'   in the analysis (i.e. concept sets) are kept. Only the name of this directory
#'   needs to be supplied here, not the full file path including the working_directory.
#'
#'   This parameter sets a default file location so the `squba` functions can easily
#'   read in relevant files without having to redefine the path each time.
#'
#' @param results_subdirectory *string* || defaults to `NULL`
#'
#'   The *subdirectory* within the base directory where any results should be output
#'   (if file = TRUE when using `argos::output_tbl`). Only the name of this directory
#'   needs to be supplied here, not the full file path including the working_directory.
#'
#' @param cdm_schema *string* || **required**
#'
#'   The name of the schema where the data in a CDM format is kept. This location
#'   must exist within the database identified in `db_conn`
#'
#' @param results_schema *string* || defaults to `NULL`
#'
#'   The name of the schema on the database where results should be output
#'   (if file = FALSE when using `argos::output_tbl`). This is also the location
#'   from which results can be retrieved if using `argos::results_tbl`
#'
#' @param vocabulary_schema *string* || defaults to `NULL`
#'
#'   The name of the schema on the database where any vocabulary reference tables
#'   (like the OMOP concept table) are kept
#'
#' @param results_tag *string* || defaults to `NULL`
#'
#'   An arbitrary suffix that will be appended onto the names of any result
#'   tables output using `argos::output_tbl`. This feature can be helpful if
#'   you are re-running analysis and want to make sure the tables are uniquely
#'   identified.
#'
#' @param cache_enabled *boolean* || defaults to `FALSE`
#'
#'   A boolean value indicating whether repeated attempts to load the same
#'   codeset (via `argos::load_codeset`) should use a cached value rather than
#'   reloading
#'
#' @param retain_intermediates *boolean* || defaults to `FALSE`
#'
#'   A boolean indicating whether intermediate/temporary tables should be
#'   manifested and retained on the database (in the defined `results_schema`)
#'   or remain as temporary objects
#'
#' @param db_trace *boolean* || defaults to `TRUE`
#'
#'   A boolean indicating whether the query log printed in the console should
#'   include detailed information about execution of SQL queries in the database.
#'   This is essentially a "verbose" parameter controlling how much information
#'   you want to see about certain queries being executed.
#'
#' @param default_file_output *boolean* || defaults to `FALSE`
#'
#'   A boolean indicating whether `argos::output_tbl` should output a file
#'   by default or if it should just output the `results_schema` on the database.
#'
#'   This can also be controlled at the function level, but this is an option for
#'   a global setting if you would like local, CSV copies of all your results.
#'
#' @return
#'   This function will quietly load all exported argos functions into the
#'   environment and establish the necessary configurations to allow them to
#'   operate. Note that the argos session itself will NOT appear in the global
#'   environment.
#'
#'
#' @examples
#' \dontrun{
#'
#' ## Create a database connection with DBI or input a file path
#' ## to a json file with connection details
#' conn_dbi <- DBI::dbConnect(drv = my_driver_func(), # insert appropriate driver
#'                            dbname = "my_dbname",
#'                            host = "my_host",
#'                            port = "my_port",
#'                            user = "my_username",
#'                            password = "my_password")
#'
#' conn_json <- "path/to/connection/file"
#'
#' ## Establish session and load appropriate convenience functions &
#' ## configurations into the environment
#'
#' initialize_dq_session(session_name = "my_session",
#'                       db_conn = conn_dbi,
#'                       is_json = FALSE,
#'                       working_directory = get_wd(),
#'                       file_subdirectory = "my_files",
#'                       cdm_schema = "my_schema")
#'
#' }
#'
#'
#' @import srcr
#' @importFrom DBI dbGetInfo
#' @importFrom stringr str_remove
#' @export
#'
initialize_dq_session <- function(session_name,
                                  db_conn,
                                  is_json = FALSE,
                                  working_directory = getwd(),
                                  file_subdirectory,
                                  results_subdirectory = NULL,
                                  cdm_schema,
                                  results_schema = NULL,
                                  vocabulary_schema = NULL,
                                  results_tag = NULL,
                                  cache_enabled = FALSE,
                                  retain_intermediates = FALSE,
                                  db_trace = TRUE,
                                  default_file_output = FALSE){

  # Establish session
  argos_session <- argos$new(session_name)

  set_argos_default(argos_session)

  # Set standard configs
  if(!is_json){
    get_argos_default()$config('db_src', db_conn)
  }else{
    get_argos_default()$config('db_src', srcr(db_conn))
  }

  get_argos_default()$config('cdm_schema', cdm_schema)
  get_argos_default()$config('results_schema', results_schema)
  get_argos_default()$config('vocabulary_schema', vocabulary_schema)
  get_argos_default()$config('cache_enabled', cache_enabled)
  get_argos_default()$config('retain_intermediates', retain_intermediates)
  get_argos_default()$config('db_trace', db_trace)
  get_argos_default()$config('can_explain', !is.na(tryCatch(db_explain(config('db_src'), 'select 1 = 1'),
                                                      error = function(e) NA)))
  get_argos_default()$config('results_target', ifelse(default_file_output, 'file', TRUE))

  if(is.null(results_tag)){
    get_argos_default()$config('results_name_tag', '')
  }else{
    get_argos_default()$config('results_name_tag', results_tag)
  }

  # Set working directory
  get_argos_default()$config('base_dir', working_directory)

  # Set specs directory
  ## Drop path to working directory if present
  specs_drop_wd <- stringr::str_remove(file_subdirectory, working_directory)
  results_drop_wd <- stringr::str_remove(results_subdirectory, working_directory)
  get_argos_default()$config('subdirs', list(spec_dir = specs_drop_wd,
                                             result_dir = results_drop_wd))

  # Print session information
  db_str <- DBI::dbGetInfo(config('db_src'))
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  cli::cli_inform(paste0('Connected to: ', db_str$dbname, '@', db_str$host))

}
