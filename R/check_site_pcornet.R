#' check for `site_summ` column and switch to `site`
#'
#' @param tbl the tbl with to use for
#' replacement of `site_summ`
#'
#'
#' @return tbl with `site` replacing `site_summ`
#'
#' @export
#'

replace_site_col_pcnt <- function(tbl) {

  alt_col <- if('site_summ' %in% colnames(tbl)){'site_summ'}else if('dmid' %in% colnames(tbl)){'dmid'}
  alt_col_exist <- if('site_summ' %in% colnames(tbl) || 'dmid' %in% colnames(tbl)){TRUE}else{FALSE}
  site_exist <- 'site' %in% colnames(tbl)
  if(alt_col_exist & ! site_exist)
  {final_tbl_site <-
    tbl %>% ungroup() %>% rename(site = !!sym(alt_col))}
  else if(alt_col_exist & site_exist)
  {final_tbl_site <-
    tbl %>% ungroup() %>% select(-!!sym(alt_col))}
  else {final_tbl_site <- tbl}

}

#' Check site type (single vs multi) against number of sites provided in list
#'
#' @param cohort a cohort tbl with a column for `site`, `person_id`, `start_date` and `end_date`
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#'
#' @return if @multi_or_single_site = single but multiple sites are provided, the cohort table
#'         is returned with a summary site column equaling `combined` so all sites will be treated
#'         as one
#'
#'         otherwise, the existing site column is left alone. if an illogical parameter combination
#'         is supplied, the function will provide an error with recommendations on how to remedy the
#'         issue.
#'
#'
#' @export
#'
check_site_type_pcnt <- function(cohort,
                                 multi_or_single_site){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if('site' %in% colnames(cohort)){

    # count number of sites in site list that also exist in the cohort
    n_site <- cohort %>% select(site) %>%
      summarise(n_distinct(site)) %>% pull()
    #filter(site %in% site_list) %>%
    #summarise(n_distinct(site)) %>% pull()

    if(multi_or_single_site == 'single' && n_site > 1){
      # create new "summary" site column / name, add that to grouped list
      # instead of site, and create new site list to account for new site name
      cohort_final <- cohort %>%
        #filter(site %in% site_list) %>%
        mutate(site_summ = 'combined')

      grouped_list <- c('site_summ')
      site_list_adj <- 'combined'

      # }else if(multi_or_single_site == 'multi' && n_site == 1){
      #
      #   cli::cli_abort('Please include data from multiple sites in your cohort to
      #                   conduct a multi-site analysis.')

    }else if((multi_or_single_site == 'single' || multi_or_single_site == 'multi' && n_site == 1) ||
             (multi_or_single_site == 'multi' && n_site > 1)){

      cohort_final <- cohort %>%
        rename('dmid' = site)
      #filter(site %in% site_list)

      site_list <- cohort_final %>%
        select(dmid) %>% distinct() %>% pull()

      grouped_list <- c('dmid')
      site_list_adj <- site_list

    }else{cli::cli_abort('Invalid argument for multi_or_single_site. Please select either {.code single} or {.code multi}')}
  }else{cli::cli_abort('Please include a {.code site} column in your cohort.')}

  final <- list('cohort' = cohort_final,
                'grouped_list' = grouped_list,
                'site_list_adj' = site_list_adj)

  return(final)
}
