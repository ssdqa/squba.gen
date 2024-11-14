#' compute age at cohort entry
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param person_tbl the CDM person table
#' @param age_groups a csv file (template found in specs folder) where the user defines the minimum and maximum
#'                   age allowed for a group and provides a string name for the group
#'
#' @return `cohort_tbl` with the age at cohort entry and age group for each patient
#'
#'

compute_age_groups <- function(cohort_tbl,
                               person_tbl,
                               age_groups) {

  cohorts <- cohort_tbl %>%
    inner_join(select(person_tbl,
                      person_id,
                      birth_date)) %>%
    mutate(age_diff = sql(calc_days_between_dates(date_col_1 = 'birth_date', date_col_2 = 'start_date')),
           age_ce = floor(age_diff/365.25)) %>%
    collect_new()

  cohorts_grpd <- cohorts %>%
    cross_join(age_groups) %>%
    mutate(age_grp = case_when(age_ce >= min_age & age_ce <= max_age ~ group,
                               TRUE ~ as.character(NA))) %>%
    filter(!is.na(age_grp)) %>%
    right_join(cohorts) %>%
    select(-c(birth_date, min_age, max_age, group)) %>%
    mutate(age_grp = case_when(is.na(age_grp) ~ 'No Group',
                               TRUE ~ age_grp))

  copy_to_new(df = cohorts_grpd)

}

#' intake codeset to customize patient labels
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param codeset_meta a CSV file with metadata relating to a codeset with customized group labels
#'
#'                     this file should have `table`, `column`, and `file_name` columns

cohort_codeset_label <- function(cohort_tbl,
                                 codeset_meta){

  codeset <- load_codeset(codeset_meta$file_name)

  filter_tbl <- select(cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id, provider_id, care_site_id) %>%
    left_join(cdm_tbl(codeset_meta$table)) %>%
    rename('concept_id' = codeset_meta$column) %>%
    inner_join(codeset, by = 'concept_id') %>%
    select(person_id, flag) %>%
    distinct() %>%
    right_join(cohort_tbl, by = 'person_id') %>%
    mutate(flag = case_when(is.na(flag) ~ 'None',
                            TRUE ~ flag))


}


#' Prepare cohort for check execution
#'
#' requirement: fields must have columns:
#' `person_id`, `start_date`, `end_date`
#'
#' @param cohort_tbl table with required fields for each member of the cohort
#' @param age_groups option to read in a CSV with age group designations to allow for stratification
#'                   by age group in output. defaults to `NULL`.
#'                   sample CSV can be found in `specs/age_group_definitions.csv`
#' @param codeset option to read in a CSV with codeset metadata to allow for labelling of
#'                cohort members based on a user-provided codeset. the codeset itself should be
#'                a CSV file with at least a `concept_id` column and a `flag` column with user-provided
#'                labels.
#'                a sample metadata CSV, where the user can provide the correct table and column information,
#'                can be found in `specs/codeset_metadata.csv`
#'
#'
#' @return a tbl with person_id and the following:
#'          `start_date` the cohort entry date
#'          `end_date` the last visit
#'          `fu`: length of follow up
#'          `site` : patient site
#'        if age_groups is not NULL:
#'          `age_ce`: patient age at cohort entry
#'          `age_grp`: user-provided age grouping
#'        if codeset is not NULL:
#'          `flag`: flag that indiciates patient is a member of a user-specified group in the codeset
#'
#' @export
#'

prepare_cohort <- function(cohort_tbl,
                           age_groups = NULL,
                           codeset = NULL) {

  ct <- cohort_tbl

  stnd <-
    ct %>%
    mutate(fu_diff = sql(calc_days_between_dates(date_col_1 = 'start_date', date_col_2 = 'end_date')),
           fu = round(fu_diff/365.25,3)) #%>%
  #select(site, person_id, start_date, end_date, fu) #%>%
  #add_site()

  if(!is.data.frame(age_groups)){
    final_age <- stnd
  }else{
    final_age <- compute_age_groups(cohort_tbl = stnd,
                                    person_tbl = cdm_tbl('person'),
                                    age_groups = age_groups)}

  if(!is.data.frame(codeset)){
    final_cdst <- stnd
  }else{
    final_cdst <- cohort_codeset_label(cohort_tbl = stnd,
                                       codeset_meta = codeset) %>%
      add_site()}

  final <- stnd %>%
    left_join(final_age) %>%
    left_join(final_cdst)

  return(final)

}
