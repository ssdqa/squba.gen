#' Join to vocabulary table
#'
#' @param tbl data table to which the vocabulary table should be joined
#' @param vocab_tbl location of the vocabulary table
#' @param col the column that should be used in the `by` statement to join
#'            to the vocab_col in the vocabulary table
#' @param vocab_col the column in the vocabulary table that should be used to join
#'                  to the @tbl
#'
#' @return the dataframe provided in @tbl with the addition of the concept name
#'         column
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
      right_join(tbl %>% rename('join_col' = col), by = c('join_col'),
                 copy = TRUE) %>%
      rename_with(~col, join_col) %>%
      collect()
  }else{
    final <- tbl %>% mutate(concept_name = 'No vocabulary table input')
  }
}


#' Generate parameter summary and recommended string to input into output function
#'
#' @param check_string abbreviation to represent check type, should be the same as what
#'                     is prefixed to the names of the output functions
#' @param ... all of the parameters input into the core function. any argument that is not
#'            able to be vectorized (i.e. a CDM tbl, codeset, etc) will not appear in the final
#'            summary
#'
#' @return output_type string to be piped into a descriptive message at the end of the core function
#'         to inform users what should be used as the `output_function` argument in the output
#'         function
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
    mutate(v = ifelse(value == TRUE, 'at', 'nt')) %>% distinct(v) %>% pull()

  output_type <- paste0(check_string, '_', site_type, '_', exp_anom, '_', time)

  # df_final <- df %>%
  #   add_row(param = 'output_function',
  #           value = output_type)

  #output_tbl(df_final, 'parameter_summary', file = TRUE)

  return(output_type)

}


#' Generate concept reference table to accompany output
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
      data_color(palette = ssdqa_colors_standard, columns = c(site)) %>%
      cols_label(denom_col = 'Total Count') %>%
      tab_header('Concept Reference Table') %>%
      opt_interactive(use_search = TRUE)
  }else{

    time_inc <- tbl %>% ungroup() %>% distinct(time_increment) %>% pull()

    t <- tbl %>%
      rename('denom_col' = denom) %>%
      distinct(site, !!sym(id_col), !!sym(name_col), denom_col) %>%
      group_by(site, !!sym(id_col), !!sym(name_col)) %>%
      mutate(denom_col = sum(denom_col)) %>%
      ungroup() %>%
      distinct() %>%
      gt::gt() %>%
      fmt_number(denom_col, decimals = 0) %>%
      data_color(palette = ssdqa_colors_standard, columns = c(site)) %>%
      cols_label(denom_col = 'Total Count (All Time Points)') %>%
      tab_header('Concept Reference Table') %>%
      opt_interactive(use_search = TRUE)

  }

  return(t)

}


#' Make ggplot2 graphical output interactive
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
#' @export
#'
make_interactive_ssdqa <- function(ggplot_obj){

  grph_meta <- ggplot_obj[['metadata']]

  if(!is.null(grph_meta)){

    if(grph_meta$pkg_backend == 'ggiraph'){

      int_grph <- girafe(ggobj = ggplot_obj)

    }else if(grph_meta$pkg_backend == 'plotly'){

      if(grph_meta$tooltip){
        int_grph <- ggplotly(p = ggplot_obj, tooltip = "text")
      }else{
        int_grph <- ggplotly(p = ggplot_obj)
      }
    }

    return(int_grph)

  }else{cli::cli_alert_warning("No interactive functionality is available for this graph")}

}
