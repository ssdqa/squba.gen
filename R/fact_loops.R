
#' Reusable FOT function
#'
#' @param cohort a cohort tbl with a column for `site`, `person_id`, `start_date` and `end_date`
#' @param check_func the base function for the check that needs to be executed across time; this argument
#'                   should be structured as the following, where dat is the input data for the function:
#'
#'                   function(dat){check_function(param1 = dat, param2 = param2_input, ...,
#'                   paramX = paramX_input)}
#'
#'                   all parameters for the base check function should be included if any defaults are not being
#'                   used
#' @param site_col the name of the column in the cohort that contains the site names
#' @param reduce_id if the check provided in @check_func returns a list of tables, this is
#'                  the ID that should be used to reduce the tables into one dataframe
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param site_list A list of sites for which you would like to examine clinical facts. Can be one site
#'                  (single-site) or multiple (multi-site)
#'
#' @return one dataframe where the output of @check_func has been executed for each @time_period in
#'         the provided @time_span for each of the sites included in @site_list
#'
#' @export
#'
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate ymd
#'
compute_fot <- function(cohort,
                        check_func,
                        site_col,
                        reduce_id = 'visit_type',
                        time_period='year',
                        time_span= c('2012-01-01','2022-12-31'),
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

#' loops through visit types and sites to compute patient facts
#'
#' @param cohort_tbl the tbl that comes from `prepare_pf`
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
#'                      to select every visit type; defaults to `pf_visit_types` in specs folder
#' @param visit_tbl the cdm visit_occurrence tbl; defaults to `cdm_tbl('visit_occurrence')`
#' @param site_list the sites to iterate through
#' @param visit_list the list of visit types to iterate through
#' @param grouped_list the input for which to group variables
#' @param domain_tbl defaults to `pf_domains` in the specs folder;
#'      @domain: the domain name; output will have this domain
#'      @default_tbl: the table to pull from
#'      @field_name the field name to filter by; leave null if no filter
#'      @field_filter: the filtered codes
#'
#' @return a returned list stratified by visit type
#'
#' @export
#'
#' @import dplyr
#' @import argos
#' @importFrom purrr reduce
#'
loop_through_visits <- function(cohort_tbl,
                                check_func,
                                site_col,
                                time=FALSE,
                                visit_type_tbl=read_codeset('pf_visit_types','ic'),
                                visit_tbl=cdm_tbl('visit_occurrence'),
                                site_list,
                                visit_list=c('inpatient','outpatient'),
                                grouped_list=c('person_id','start_date','end_date',
                                                'fu','site'),
                                domain_tbl=read_codeset('pf_domains_short','cccc')) {

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
