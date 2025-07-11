#' Join to OMOP Vocabulary Table
#'
#' This is a convenience function that allows users to join to an OMOP
#' vocabulary table (most likely the `concept` table)
#'
#' @param tbl data table to which the vocabulary table should be joined
#' @param vocab_tbl location of the vocabulary table
#' @param col the column that should be used in the `by` statement to join
#'            to the vocab_col in the vocabulary table
#' @param vocab_col the column in the vocabulary table that should be used to join
#'                  to the `tbl`
#'
#' @return the dataframe provided in `tbl` with the addition of the concept name
#'         column
#'
#' @examples
#' \dontrun{
#'
#' sample_input_tbl <- dplyr::tibble('concept' = c(1234, 5678, 91011),
#'                                   'ct_concept' = c(100, 200, 300),
#'                                   'site' = c('Site A', 'Site A', 'Site A'))
#'
#' join_to_vocabulary(tbl = sample_input_tbl,
#'                    vocab_tbl = vocabulary_tbl('concept'),
#'                    col = 'concept',
#'                    vocab_col = 'concept_id')
#' }
#'
#'
#' @export
#'
join_to_vocabulary <- function(tbl,
                               vocab_tbl,
                               col,
                               vocab_col = 'concept_id'){
  if(!is.null(vocab_tbl)){
    final <- select(vocab_tbl, vocab_col, concept_name, vocabulary_id) %>%
      rename('join_col' = vocab_col) %>%
      right_join(tbl %>% rename('join_col' = col),
                 copy = TRUE) %>%
      rename_with(~col, join_col) %>%
      collect()
  }else{
    if('concept_name' %in% colnames(tbl)){
      final <- tbl
    }else{
      final <- tbl %>% mutate(concept_name = 'No vocabulary table input')
    }
  }
}


#' Generate Parameter Summary
#'
#' This function will summarize the input parameters for the *_process functions, which is
#' then used to output a string to the console that indicates the appropriate
#' `output_function` to use in the *_output step
#'
#' @param check_string abbreviation to represent check type, should be the same as what
#'                     is prefixed to the names of the output functions
#' @param ... all of the parameters input into the core function. any argument that is not
#'            able to be vectorized (i.e. a CDM tbl, codeset, etc) will not appear in the final
#'            summary
#'
#' @return a vector with information about which parameters will be required to use
#'         the *_output function for each module; intended to be fed into a cli message
#'         command to generate a console message
#'
#' @examples
#' # intended for use inside the *_process functions
#'
#' param_summ(check_string = 'evp',
#'            multi_or_single_site = 'single',
#'            anomaly_or_exploratory = 'exploratory',
#'            time = FALSE)
#'
#' @export
#'
param_summ <- function(check_string, ...){

  argg <- c(...)


  df <- stack(argg) %>%
    rename('param' = ind,
           'value' = values)

  site_type <- df %>% filter(param == 'multi_or_single_site') %>%
    mutate(v = ifelse(value == 'single', 'ss', 'ms')) %>% distinct(v) %>% pull()
  exp_anom <- df %>% filter(param == 'anomaly_or_exploratory') %>%
    mutate(v = ifelse(value == 'anomaly', 'anom', 'exp')) %>% distinct(v) %>% pull()
  time <- df %>% filter(param == 'time') %>%
    mutate(v = ifelse(value == TRUE, 'la', 'cs')) %>% distinct(v) %>% pull()

  output_type <- paste0(site_type, '_', exp_anom, '_', time)
  output_string <- paste0(check_string, '_', site_type, '_', exp_anom, '_', time)

  output_ref <- readr::read_csv(paste0(system.file("extdata", package = 'squba.gen'),
                                       '/output_parameter_reqs.csv'))

  output_vector <- output_ref %>%
    filter(module == check_string & check == output_type) %>%
    select(-c(module, check)) %>%
    pivot_longer(cols = 1:3) %>%
    filter(!is.na(value)) %>%
    mutate(name_cat = paste0(paste0(cli::style_bold(cli::col_blue(name)), ": ", value))) %>%
    pull(name_cat)

  opt_list <- list('vector' = output_vector,
                   'string' = output_string)

  return(opt_list)

  # cli::boxx(c('You can optionally use this dataframe in the accompanying',
  # '`scv_output` function. Here are the parameters you will need:', '', vec1, '',
  # 'See ?scv_output for more details.'), padding = c(0,1,0,1),
  # header = cli::col_cyan('Output Function Details'))

}


#' Create Concept Reference Table
#'
#' For several SQUBA modules, this function is used to create a summary reference
#' table that displays a more detailed breakdown of concept usage
#'
#' @param tbl intermediate table generated in the output function that contains the concepts
#'            of interest to be displayed in the reference table
#' @param id_col the name of the column with the concept that needs to be summarised in the
#'               reference table
#' @param name_col the name of the column with the concept name associated with the concept in
#'                 `id_col`
#' @param denom the denominator count associated with @id_col to be displayed in the
#'              reference table
#' @param time logical to define whether @tbl has over time output or not
#'
#' @return a reference table with summary information about the codes in the output that
#'         could not be displayed in the associated graph
#'
#' @examples
#' # generate reference table for non-time dependent concept summary
#'
#' input_tbl_notime <- dplyr::tibble('concept_id' = c(1, 2, 3, 4),
#'                                   'concept_name' = c('test1', 'test2',
#'                                                      'test3', 'test4'),
#'                                   'ct_concept' = c(100, 200, 300, 400),
#'                                   'site' = c('Site A', 'Site A', 'Site A',
#'                                   'Site A'))
#'
#' generate_ref_table(tbl = input_tbl_notime,
#'                    id_col = 'concept_id',
#'                    name_col = 'concept_name',
#'                    denom = 'ct_concept',
#'                    time = FALSE)
#'
#' # generate reference table for time dependent concept summary
#'
#' input_tbl_time <- dplyr::tibble('concept_id' = c(1, 2, 3, 4, 1, 2, 3, 4),
#'                                 'time_start' = c('2012-01-01', '2012-01-01',
#'                                                  '2012-01-01', '2012-01-01',
#'                                                  '2013-01-01', '2013-01-01',
#'                                                  '2013-01-01', '2013-01-01'),
#'                                 'time_increment' = c('year','year','year',
#'                                                      'year','year','year',
#'                                                      'year','year'),
#'                                 'concept_name' = c('test1', 'test2', 'test3',
#'                                                    'test4', 'test1', 'test2',
#'                                                    'test3', 'test4'),
#'                                 'ct_concept' = c(100, 200, 300, 400, 200,
#'                                                  300, 400, 500),
#'                                 'site' = c('Site A', 'Site A', 'Site A',
#'                                            'Site A', 'Site A', 'Site A',
#'                                            'Site A', 'Site A'))
#'
#' generate_ref_table(tbl = input_tbl_time,
#'                    id_col = 'concept_id',
#'                    name_col = 'concept_name',
#'                    denom = 'ct_concept',
#'                    time = TRUE)
#'
#'
#' @export
#'
#' @import gt
#'

generate_ref_table <- function(tbl,
                               id_col,
                               name_col,
                               denom,
                               time = FALSE){
  if(!time){

    t <- tbl %>%
      rename('denom_col' = denom) %>%
      distinct(site, !!sym(id_col), !!sym(name_col), denom_col) %>%
      gt::gt() %>%
      fmt_number(denom_col, decimals = 0) %>%
      data_color(palette = squba_colors_standard, columns = c(site)) %>%
      cols_label(denom_col = 'Total Count') %>%
      tab_header('Concept Reference Table') %>%
      opt_interactive(use_search = TRUE)
  }else{

    time_inc <- tbl %>% ungroup() %>% distinct(time_increment) %>% pull()

    t <- tbl %>%
      rename('denom_col' = denom) %>%
      distinct(site, time_start, !!sym(id_col), !!sym(name_col), denom_col) %>%
      group_by(across(-c(denom_col, time_start))) %>%
      summarise(denom_col = sum(denom_col)) %>%
      ungroup() %>%
      distinct() %>%
      gt::gt() %>%
      fmt_number(denom_col, decimals = 0) %>%
      data_color(palette = squba_colors_standard, columns = c(site)) %>%
      cols_label(denom_col = 'Total Count (All Time Points)') %>%
      tab_header('Concept Reference Table') %>%
      opt_interactive(use_search = TRUE)

  }

  return(t)

}


#' Create Interactive Graphical Output
#'
#' This function converts a ggplot object output by any of the *_output functions
#' into an interactive ggiraph or plotly object.
#'
#' @param ggplot_obj a ggplot object output by any of the `*_output` functions native
#'                   to each module
#'
#' @return the same graph with interactive functionality and pre-set tooltips
#'         based either in the `ggiraph` package or the `plotly` package
#'
#' @importFrom ggiraph girafe
#' @importFrom plotly ggplotly
#'
#' @examples
#' \dontrun{
#' # first, execute the *_output function of interest to build a graph
#' grph_output <- *_output(process_output = my_df,
#'                         ...)
#'
#' # then, run make_interactive_squba to activate interactive functionality
#' # some graphs may not have interactive abilities, which will be communicated
#' # via a message in the console
#'
#' make_interactive_squba(grph_output)
#'
#' }
#'
#'
#' @export
#'
make_interactive_squba <- function(ggplot_obj){

  if(any(class(ggplot_obj) %in% 'patchwork')){
    grph_meta <- ggplot_obj[[1]][['metadata']]
  }else{
    grph_meta <- ggplot_obj[['metadata']]
  }

  if(!is.null(grph_meta)){

    if(grph_meta$pkg_backend == 'ggiraph'){

      int_grph <- girafe(ggobj = ggplot_obj)

    }else if(grph_meta$pkg_backend == 'plotly'){

      if(grph_meta$tooltip){
        int_grph <- ggplotly(p = ggplot_obj, tooltip = "text")
      }else{
        int_grph <- ggplotly(p = ggplot_obj)
      }
    }else if(grph_meta$pkg_backend == 'plotly_ssc'){

      ggplot_obj_edit <- ggplot_obj + guides(shape = 'none')

      int_grph <- ggplotly(p = ggplot_obj_edit, tooltip = 'text')
    }

    return(int_grph)

  }else{cli::cli_alert_warning("No interactive functionality is available for this graph")}

}
