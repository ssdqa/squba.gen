
#' Compute Facts Over Time
#'
#' This function will loop through a time series and execute a user-provided
#' function against only the data within the time period of interest. It will
#' also ensure that only patients whose start date has passed but whose end date
#' has not passed are included in the analysis for any given time period. This means
#' that, for example, a patient who has a cohort period 1/1/2020 to 1/1/2025 would
#' be included when looking at the year of 2024, but they would not be included when
#' looking at the year of 2019 as they had not yet entered the cohort.
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
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param check_func *function* || **required**
#'
#'   The function that should be executed within each time period in the loop. This
#'   parameter should be structured as the following, where `dat` is the input data
#'   for the function:
#'
#'   `function(dat){check_function(param1 = dat, param2 = param2_input, ..., paramX = paramX_input)}`
#'
#'   Make sure to include all parameters required for the original function where
#'   default values are not being used.
#'
#' @param site_col *string* || **required**
#'
#'   The name of the column in the cohort table that contains the site names. This
#'   will typically be either `site` or `site_summ`
#'
#' @param reduce_id *string* || defaults to `NULL`
#'
#'   If the function provided in `check_func` returns a list of tables, this parameter
#'   should be the name of the column that should be used to reduce the tables into
#'   one dataframe (via [dplyr::bind_rows])
#'
#' @param time_period *string* || defaults to `year`
#'
#'   A string indicating the distance between dates within the specified time_span.
#'   Defaults to `year`, but other time periods such as `month` or `week` are
#'   also acceptable
#'
#' @param time_span *vector - length 2* || defaults to `c('2012-01-01', '2020-01-01')`
#'
#'   A vector indicating the lower and upper bounds of the time series for longitudinal analyses
#'
#' @param site_list *list* || **required**
#'
#'   A list of sites for which you would like to examine clinical facts. Can be one site
#'   (single-site) or multiple (multi-site). Ensure that all sites listed here exist
#'   in the provided cohort table.
#'
#' @return
#'   This function will return a dataframe where the output of the provided `check_func`
#'   has been executed once for each `time_period` in the provided `time_span` for each of
#'   the sites included in `site_list`
#'
#' @examples
#' \dontrun{
#' # for a function like...
#'  mock_function <- function(cohort,
#'                            input_tbl){
#'
#'      test <- input_tbl %>%
#'         inner_join(cohort) %>%
#'         group_by(site) %>%
#'         summarise(n_row = n())
#'
#'      return(test)
#'  }
#'
#' # allow this function to be executed for each time period in a series
#' cohort <- dplyr::tibble('person_id' = c(1,2,3,4),
#'                         'site' = c('Site A', 'Site A', 'Site B', 'Site B'),
#'                         'start_date' = c('2012-01-01', '2014-01-10',
#'                                          '2015-03-05, '2020-01-07'),
#'                         'end_date' = c('2015-01-01', '2019-01-10',
#'                                        '2010-03-05, '2024-01-07'))
#'
#' compute_fot(cohort = cohort,
#'             check_func = function(dat){
#'               mock_function(cohort = dat,
#'                             input_tbl = cdm_tbl('condition_occurrence'))
#'             },
#'             site_col = 'site',
#'             reduce_id = NULL,
#'             time_period = 'year',
#'             time_span = c('2012-01-01','2022-12-31'),
#'             site_list = c('Site A', 'Site B'))
#'
#'
#' }
#'
#' @export
#'
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate ymd
#'
compute_fot <- function(cohort,
                        check_func,
                        site_col,
                        reduce_id = NULL,
                        time_period = 'year',
                        time_span= c('2012-01-01','2020-12-31'),
                        site_list
) {

  site_list_v <- unlist(site_list)

  final_results <- list()

  t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
  t2 <- ceiling_date(t1, time_period) - 1


  # narrows the visit time to cohort_entry and end date
  for(k in 1:length(t1)) {

    message(paste0('Starting ',t1[[k]]))

    target <- ymd(t1[[k]])

    baseline_start_date <- target
    baseline_end_date <- ceiling_date(target, time_period) - 1

    cohort_narrowed <- cohort %>%
      filter(start_date <= baseline_end_date,
             baseline_start_date <= end_date)
    # mutate(start_date = as_date(baseline_start_date),
    #        end_date = as_date(baseline_end_date))


    cohort_narrow_prepped <- cohort_narrowed %>%
      filter(!! sym(site_col) %in% site_list_v) %>%
      mutate(time_start=baseline_start_date,
             time_end=baseline_end_date,
             time_increment=time_period)

    output <- check_func(dat = cohort_narrow_prepped)

    if(is.list(output)&!any(class(output)=='tbl_sql')){
      output_reduced <- dplyr::bind_rows(output, .id= reduce_id)
    }else{output_reduced <- output}

    final_results[[k]] <- output_reduced

  }

  rslt = reduce(.x=final_results,
                .f=dplyr::union)

  return(rslt)

}

#' Patient Facts per Visit Type -- PCORnet
#'
#' @param cohort_tbl a table with members of the cohort that has been run through [prepare_cohort()]
#' @param check_func the base function for the check that needs to be executed across time; this argument
#'                   should be structured as the following, where cht is the cohort and t is the input data
#'                   for the function:
#'
#'                   function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ...,
#'                   paramX = paramX_input)}
#'
#'                   all parameters for the base check function should be included if any defaults are not being
#'                   used
#' @param site_col the column in the data where the site variable can be found
#' @param time a logical indicating whether the analysis is being conducted longitudinally
#' @param visit_type_tbl The visit_concept_ids of interest for the analysis. `all` may be used in this field
#'                      to select every visit type
#' @param visit_type_tbl a table that defines available visit types that are called in `visit_list.`
#' - `enc_type`: enc_type that represents the visit type of interest (i.e. 9201 or IP)
#' - `visit_type`: the string label to describe the visit type; this label can be used multiple times
#'                 within the file if multiple visit_concept_ids/enc_types represent the visit type
#' @param visit_tbl the cdm encounter tbl
#' @param site_list the sites to iterate through
#' @param visit_list the list of visit types to iterate through
#' @param domain_tbl a table that defines the domains where facts should be identified with the following columns:
#' - `domain`: a string label for the domain being examined (i.e. prescription drugs)
#' - `domain_tbl`: the CDM table where information for this domain can be found (i.e. drug_exposure)
#' - `filter_logic`: an optional string to be parsed as logic to filter the domain_tbl as needed to best represent the domain
#'
#' @return a list of dataframes with median number of facts per patient for all domains in domain_tbl, where each dataframe is specific to a
#'         given visit type from visit_list
#'
#' @keywords internal
#'
loop_through_visits_pcnt <- function(cohort_tbl,
                                     check_func,
                                     site_col,
                                     time=FALSE,
                                     visit_type_tbl,
                                     visit_tbl=cdm_tbl('encounter'),
                                     site_list,
                                     visit_list=c('inpatient','outpatient'),
                                     domain_tbl) {

  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {

    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {

      site_list_thisrnd <- site_list[[k]]

      # filters by site
      cohort_site <- cohort_tbl %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

      # pulls the visit_concept_id's that correspond to the visit_list
      visit_types <-
        visit_type_tbl %>%
        filter(visit_type %in% c(visit_list[[j]])) %>%
        select(enc_type) %>% pull()

      # narrows the visit time to cohort_entry and end date
      visits <-
        cohort_site %>%
        inner_join(
          select(visit_tbl,
                 patid,
                 encounterid,
                 enc_type,
                 admit_date)
        ) %>%
        filter(enc_type %in% c(visit_types)) %>%
        filter(admit_date >= start_date,
               admit_date <= end_date) %>%
        compute_new(temporary=TRUE,
                    indexes=list('patid'))

      if(time){visits <- visits %>% filter(admit_date >= time_start,
                                           admit_date <= time_end)}

      # execute function
      domain_compute <- check_func(cht = cohort_site,
                                   t = visits)

      site_output[[k]] <- domain_compute

    }


    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)

    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[visit_list[j]]] <- all_site

  }

  visit_output

}


#' Patient Facts per Visit Type -- OMOP
#'
#' @param cohort_tbl a table with members of the cohort that has been run through [prepare_cohort()]
#' @param check_func the base function for the check that needs to be executed across time; this argument
#'                   should be structured as the following, where cht is the cohort and t is the input data
#'                   for the function:
#'
#'                   function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ...,
#'                   paramX = paramX_input)}
#'
#'                   all parameters for the base check function should be included if any defaults are not being
#'                   used
#' @param site_col the column in the data where the site variable can be found
#' @param time a logical indicating whether the analysis is being conducted longitudinally
#' @param visit_type_tbl a table that defines available visit types that are called in `visit_types.`:
#' - `visit_concept_id` / `visit_detail_concept_id`: the visit_(detail)_concept_id that represents the visit type of interest (i.e. 9201 or IP)
#' - `visit_type`: the string label to describe the visit type; this label can be used multiple times
#'                 within the file if multiple visit_concept_ids/enc_types represent the visit type
#' @param visit_tbl the cdm visit_occurrence or visit_detail tbl
#' @param site_list the sites to iterate through
#' @param visit_list the list of visit types to iterate through
#' @param domain_tbl a table that defines the domains where facts should be identified with the following columns:
#' - `domain`: a string label for the domain being examined (i.e. prescription drugs)
#' - `domain_tbl`: the CDM table where information for this domain can be found (i.e. drug_exposure)
#' - `filter_logic`: an optional string to be parsed as logic to filter the domain_tbl as needed to best represent the domain
#'
#' @return a list of dataframes with median number of facts per patient for all domains in domain_tbl, where each dataframe is specific to a
#'         given visit type from visit_list
#'
#' @import dplyr
#' @import argos
#' @importFrom purrr reduce
#'
#' @keywords internal
#'
loop_through_visits_omop <- function(cohort_tbl,
                                     check_func,
                                     site_col,
                                     time=FALSE,
                                     visit_type_tbl,
                                     visit_tbl=cdm_tbl('visit_occurrence'),
                                     site_list,
                                     visit_list=c('inpatient','outpatient'),
                                     domain_tbl) {

  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {

    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {

      site_list_thisrnd <- site_list[[k]]

      # filters by site
      cohort_site <- cohort_tbl %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

      if(any(names(visit_tbl) == 'visit_detail_id')){
        # pulls the visit_detail_concept_id's that correspond to the visit_list
        visit_types <-
          visit_type_tbl %>%
          filter(visit_type %in% c(visit_list[[j]])) %>%
          select(visit_detail_concept_id) %>% pull()

        # narrows the visit time to cohort_entry and end date
        visits <-
          cohort_site %>%
          inner_join(
            select(visit_tbl,
                   person_id,
                   visit_detail_id,
                   visit_occurrence_id,
                   visit_detail_concept_id,
                   visit_detail_start_date)
          ) %>%
          filter(visit_detail_concept_id %in% c(visit_types)) %>%
          filter(visit_detail_start_date >= start_date,
                 visit_detail_start_date <= end_date) %>%
          compute_new()

        if(time){visits <- visits %>% filter(visit_detail_start_date >= time_start,
                                             visit_detail_start_date <= time_end)}

      }else{
        # pulls the visit_concept_id's that correspond to the visit_list
        visit_types <-
          visit_type_tbl %>%
          filter(visit_type %in% c(visit_list[[j]])) %>%
          select(visit_concept_id) %>% pull()

        # narrows the visit time to cohort_entry and end date
        visits <-
          cohort_site %>%
          inner_join(
            select(visit_tbl,
                   person_id,
                   visit_occurrence_id,
                   visit_concept_id,
                   visit_start_date)
          ) %>%
          filter(visit_concept_id %in% c(visit_types)) %>%
          filter(visit_start_date >= start_date,
                 visit_start_date <= end_date) %>%
          compute_new()

        if(time){visits <- visits %>% filter(visit_start_date >= time_start,
                                             visit_start_date <= time_end)}

      }

      # execute function
      domain_compute <- check_func(cht = cohort_site,
                                   t = visits)

      site_output[[k]] <- domain_compute

    }


    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)

    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[visit_list[j]]] <- all_site

  }

  visit_output

}


#' Patient Facts per Visit Type
#'
#' This function will loop through each site and visit type provided and execute
#' the provided check function to compute facts stratified by visit type. Primarily intended
#' for use in the Patient Facts module, but can be adapted for use elsewhere.
#'
#' @param cohort_tbl *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module. It is recommended that
#'   this table has been passed through the [prepare_cohort] function as well.
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#'    - `omop`: run the [loop_through_visits_omop()] function against an OMOP CDM instance
#'    - `pcornet`: run the [loop_through_visits_pcnt()] function against a PCORnet CDM instance
#'
#' @param check_func *function* || **required**
#'
#'   The function that should be executed within each time period in the loop. This
#'   parameter should be structured as the following, where `cht` is the cohort table
#'   and `t` is the input data for the function:
#'
#'   `function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ..., paramX = paramX_input)}`
#'
#'   Make sure to include all parameters required for the original function where
#'   default values are not being used.
#'
#' @param site_col *string* || **required**
#'
#'   The name of the column in the cohort table that contains the site names. This
#'   will typically be either `site` or `site_summ`
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'   A boolean to indicate whether to execute a longitudinal analysis
#'
#' @param visit_type_tbl *tabular input* || **required**
#'
#'   A table that defines visit types of interest called in `visit_list.` This input
#'   should contain:
#'   - `visit_concept_id` / `visit_detail_concept_id` or `enc_type` | *integer* or *character* | the `visit_(detail)_concept_id` or `enc_type` that represents the visit type of interest (i.e. 9201 or IP)
#'   - `visit_type` | *character* | the string label to describe the visit type
#'
#' @param visit_tbl *tabular input* || defaults to `cdm_tbl('visit_occurrence')`
#'
#'   The CDM table with visit information (i.e. visit_occurrence or encounter)
#'
#' @param site_list *list* || **required**
#'
#'   A list of sites for which you would like to examine clinical facts. Can be one site
#'   (single-site) or multiple (multi-site). Ensure that all sites listed here exist
#'   in the provided cohort table.
#'
#' @param visit_list *string or vector* || defaults to `c('outpatient', 'inpatient')`
#'
#'   A string or vector of visit types by which the output should be stratified. Each
#'   visit type listed in this parameter should match an associated visit type defined
#'   in the `visit_type_tbl`
#'
#' @param domain_tbl *tabular input* || **required**
#'
#'   A table that defines the fact domains to be investigated in the analysis. This
#'   input should contain:
#'   - `domain` | *character* | a string label for the domain being examined (i.e. prescription drugs)
#'   - `domain_tbl` | *character* | the CDM table where information for this domain can be found (i.e. drug_exposure)
#'   - `filter_logic` | *character* | logic to be applied to the domain_tbl in order to achieve the definition of interest; should be written as if you were applying it in a dplyr::filter command in R
#'
#' @return
#'   This function will return a list of dataframes with the median number of facts per
#'   patient for all domains in `domain_tbl`, where each dataframe is specific to a
#'   given visit type from `visit_list`
#'
#' @examples
#' \dontrun{
#' # for a function like...
#'  mock_function <- function(cohort,
#'                            input_tbl){
#'
#'      test <- input_tbl %>%
#'         inner_join(cohort) %>%
#'         group_by(site) %>%
#'         summarise(n_row = n())
#'
#'      return(test)
#'  }
#'
#' # allow this function to be executed while stratifying by visit type
#' cohort <- dplyr::tibble('person_id' = c(1,2,3,4),
#'                         'site' = c('Site A', 'Site A', 'Site B', 'Site B'),
#'                         'start_date' = c('2012-01-01', '2014-01-10',
#'                                          '2015-03-05, '2020-01-07'),
#'                         'end_date' = c('2015-01-01', '2019-01-10',
#'                                        '2010-03-05, '2024-01-07'))
#'
#' sample_visits <- dplyr::tibble('visit_concept_id' = 9202,
#'                                'visit_type' = 'outpatient')
#'
#' sample_domains <- dplyr::tibble('domain' = 'diagnosis',
#'                                 'domain_tbl' = 'condition_occurrence',
#'                                 'filter_logic' = NA)
#'
#' loop_through_visits(cohort_tbl = cohort,
#'                     omop_or_pcornet = 'omop',
#'                     check_func = function(cht, t){
#'                        mock_function(cohort = cht,
#'                                      input_tbl = t)
#'                     },
#'                     site_col = 'site',
#'                     time = FALSE,
#'                     visit_type_tbl = sample_visits,
#'                     visit_tbl = cdm_tbl('visit_occurrence'),
#'                     site_list = c('Site A', 'Site B'),
#'                     visit_list = c('outpatient'),
#'                     domain_tbl = sample_domains)
#'
#'
#' }
#'
#' @export
#'
loop_through_visits <- function(cohort_tbl,
                                omop_or_pcornet,
                                check_func,
                                site_col,
                                time=FALSE,
                                visit_type_tbl,
                                visit_tbl,
                                site_list,
                                visit_list=c('outpatient', 'inpatient'),
                                domain_tbl) {

  if(omop_or_pcornet == 'omop'){
    rslt <- loop_through_visits_omop(cohort_tbl = cohort_tbl,
                                     check_func = check_func,
                                     site_col = site_col,
                                     time = time,
                                     visit_type_tbl = visit_type_tbl,
                                     visit_tbl = visit_tbl,
                                     site_list = site_list,
                                     visit_list = visit_list,
                                     domain_tbl = domain_tbl)
  }else if(omop_or_pcornet == 'pcornet'){
    rslt <- loop_through_visits_pcnt(cohort_tbl = cohort_tbl,
                                     check_func = check_func,
                                     site_col = site_col,
                                     time = time,
                                     visit_type_tbl = visit_type_tbl,
                                     visit_tbl = visit_tbl,
                                     site_list = site_list,
                                     visit_list = visit_list,
                                     domain_tbl = domain_tbl)
  }else{cli::cli_abort('Please input either `omop` or `pcornet` as your CDM of choice.')}

  return(rslt)
}
