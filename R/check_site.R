
#' Check Site Type
#'
#' This function will evaluate the number of distinct values in the `site` column in
#' the provided cohort table and determine how that compares to the provided
#' `multi_or_single_site` designation.
#'
#' @param cohort *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @return
#'   If `multi_or_single_site = single` but multiple sites are provided, the cohort table
#'   is returned with a summary site column set to `combined` so all sites will be treated
#'   as one group. Otherwise, the existing site column is returned as-is.
#'   If an illogical parameter combination is supplied, the function will
#'   return an error with recommendations on how to remedy the issue.
#'
#' @examples
#' ## Create sample cohort
#' cohort_sample <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
#'                                person_id = c(1,2,3))
#'
#' ## If number of sites & indicated multi/single site match, output same table
#' check_site_type(cohort = cohort_sample,
#'                 multi_or_single_site = 'multi')
#'
#' ## If multiple sites but single site indicated, create site_summ column
#' check_site_type(cohort = cohort_sample,
#'                 multi_or_single_site = 'single')
#'
#' @export
#'
check_site_type <- function(cohort,
                            multi_or_single_site){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if('site' %in% colnames(cohort)){

    # count number of sites in site list that also exist in the cohort
    n_site <- cohort %>% select(site) %>%
      summarise(nst = n_distinct(site)) %>% pull(nst)

    if(multi_or_single_site == 'single' && n_site > 1){
      # create new "summary" site column / name, add that to grouped list
      # instead of site, and create new site list to account for new site name
      cohort_final <- cohort %>%
        mutate(site_summ = 'combined')

      grouped_list <- c('site_summ')
      site_list_adj <- 'combined'

    }else if(multi_or_single_site == 'multi' && n_site == 1){

      cli::cli_abort('Please include data from multiple sites in your cohort to
                      conduct a multi-site analysis.')

    }else if((multi_or_single_site == 'single' && n_site == 1) ||
             (multi_or_single_site == 'multi' && n_site > 1)){

      cohort_final <- cohort

      site_list <- cohort %>%
        select(site) %>% distinct() %>% pull()

      grouped_list <- c('site')
      site_list_adj <- site_list

    }else{cli::cli_abort('Invalid argument for multi_or_single_site. Please select either {.code single} or {.code multi}')}
  }else{cli::cli_abort('Please include a {.code site} column in your cohort.')}

  final <- list('cohort' = cohort_final,
                'grouped_list' = grouped_list,
                'site_list_adj' = site_list_adj)

  return(final)
}

#' Replace Summary Site Column
#'
#' For analyses where a summary site column was created by [check_site_type()],
#' this function will replace the name of that column with the original "site"
#' name.
#'
#' @param tbl *tabular input* || **required**
#'
#'   The table with a `site_summ` column that needs to be replaced
#'
#' @return
#'   The same table that was used as input, with `site` replacing `site_summ`
#'
#' @examples
#' ## Sample input table
#' input_sample <- dplyr::tibble(person_id = c(1,2,3),
#'                               site_summ = c("combined","combined","combined"))
#'
#' ## Replace site_summ col for final output
#' replace_site_col(tbl = input_sample)
#'
#' @export

replace_site_col <- function(tbl) {

  site_summ_exist <- 'site_summ' %in% colnames(tbl)
  site_exist <- 'site' %in% colnames(tbl)
  if(site_summ_exist & ! site_exist)
  {final_tbl_site <-
    tbl %>% ungroup() %>% rename(site = site_summ)}
  else if(site_summ_exist & site_exist)
  {final_tbl_site <-
    tbl %>% ungroup() %>% select(-site_summ)}
  else {final_tbl_site <- tbl}

}
