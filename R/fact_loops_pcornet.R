
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
loop_through_visits_pcnt <- function(cohort_tbl,
                                     check_func,
                                     site_col,
                                     time=FALSE,
                                     visit_type_tbl=read_codeset('pf_visit_types','ic'),
                                     visit_tbl=cdm_tbl('encounter'),
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
