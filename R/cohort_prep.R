#' Age at Cohort Entry -- OMOP
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param person_tbl the CDM person table
#' @param age_groups a table where the user defines the minimum and maximum
#'                   age allowed for a group and provides a string name for the group
#'
#' @return `cohort_tbl` with the age at cohort entry and age group for each patient
#'
#'
compute_age_groups_omop <- function(cohort_tbl,
                                    person_tbl,
                                    age_groups) {

  new_person <- build_birth_date(cohort = cohort_tbl,
                                 person_tbl = person_tbl)

  cohorts <- new_person %>%
    mutate(age_diff = as.numeric(as.Date(start_date) - birth_date),
           age_ce = floor(age_diff/365.25)) %>%
    collect()

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

#' Age at Cohort Entry -- PCORnet
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param person_tbl the CDM person table
#' @param age_groups a table where the user defines the minimum and maximum
#'                   age allowed for a group and provides a string name for the group
#'
#' @return `cohort_tbl` with the age at cohort entry and age group for each patient
#'
#'
compute_age_groups_pcnt <- function(cohort_tbl,
                                    person_tbl,
                                    age_groups) {

  cohorts <- cohort_tbl %>%
    inner_join(select(person_tbl,
                      patid,
                      birth_date)) %>%
    mutate(age_diff = sql(calc_days_between_dates(date_col_1 = 'birth_date', date_col_2 = 'start_date')),
           age_ce = floor(age_diff/365.25)) %>%
    collect()

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


# cohort_codeset_label_pcnt <- function(cohort_tbl,
#                                       codeset_meta){
#
#   codeset <- load_codeset(codeset_meta$file_name)
#
#   filter_tbl <- select(cdm_tbl('encounter'), patid, encounterid, providerid, facilityid) %>%
#     left_join(cdm_tbl(codeset_meta$table)) %>%
#     rename('concept_id' = codeset_meta$column) %>%
#     inner_join(codeset, by = 'concept_id') %>%
#     select(person_id, flag) %>%
#     distinct() %>%
#     right_join(cohort_tbl, by = 'person_id') %>%
#     mutate(flag = case_when(is.na(flag) ~ 'None',
#                             TRUE ~ flag))
#
#
# }


# cohort_codeset_label_omop <- function(cohort_tbl,
#                                       codeset_meta){
#
#   codeset <- load_codeset(codeset_meta$file_name)
#
#   filter_tbl <- select(cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id, provider_id, care_site_id) %>%
#     left_join(cdm_tbl(codeset_meta$table)) %>%
#     rename('concept_id' = codeset_meta$column) %>%
#     inner_join(codeset, by = 'concept_id') %>%
#     select(person_id, flag) %>%
#     distinct() %>%
#     right_join(cohort_tbl, by = 'person_id') %>%
#     mutate(flag = case_when(is.na(flag) ~ 'None',
#                             TRUE ~ flag))
#
#
# }


#' Prepare Cohort
#'
#' For the cohort table input into any of the *_process functions, this function
#' will prepare that cohort for the rest of the analysis. This includes computing
#' follow-up based on start and end dates and computing age at cohort entry categorized
#' into user-provided groups. Note that the cohort table must have columns:
#' `site`, `person_id`, `start_date`, `end_date`
#'
#' @param cohort_tbl table with required fields for each member of the cohort
#' @param age_groups option to read in a table with age group designations to allow for stratification
#'                   by age group in output. defaults to `NULL`.
#' @param codeset option to read in a table with codeset metadata to allow for labelling of
#'                cohort members based on a user-provided codeset. the codeset itself should be
#'                a table with at least a `concept_id` column and a `flag` column with user-provided
#'                labels.
#' @param omop_or_pcornet string indicating the appropriate backend CDM (`omop` or `pcornet`)
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
#' @examples
#' \dontrun{
#' sample_cohort <- dplyr::tibble(site = c('Site A', 'Site B'),
#'                                person_id = c(1, 2),
#'                                start_date = c(as.Date('2012-01-01'),
#'                                               as.Date('2014-02-01)),
#'                                end_date = c(as.Date('2015-01-01'),
#'                                             as.Date('2020-02-01)))
#'
#' age_grp_tbl <- dplyr::tibble(min_age = c(0, 7),
#'                              max_age = c(6, 10),
#'                              group = c('0-6', '7-10'))
#'
#' prepare_cohort(cohort_tbl = sample_cohort,
#'                age_groups = age_grp_tbl,
#'                omop_or_pcornet = 'omop')
#'
#' }
#'
#'
#' @export
#'
prepare_cohort <- function(cohort_tbl,
                           age_groups = NULL,
                           codeset = NULL,
                           omop_or_pcornet) {

  ct <- copy_to_new(df = cohort_tbl)

  stnd <-
    ct %>%
    mutate(fu_diff = sql(calc_days_between_dates(date_col_1 = 'start_date', date_col_2 = 'end_date')),
           fu_diff = as.numeric(fu_diff),
           fu = round(fu_diff/365.25,3))

  if(!is.data.frame(age_groups)){
    final_age <- stnd
  }else{
    if(omop_or_pcornet == 'omop'){
      final_age <- compute_age_groups_omop(cohort_tbl = stnd,
                                           person_tbl = cdm_tbl('person'),
                                           age_groups = age_groups)
    }else if(omop_or_pcornet == 'pcornet'){
      final_age <- compute_age_groups_pcnt(cohort_tbl = stnd,
                                           person_tbl = cdm_tbl('demographic'),
                                           age_groups = age_groups)
      }

    }

  # if(!is.data.frame(codeset)){
  #   final_cdst <- stnd
  # }else{
  #   if(omop_or_pcornet == 'omop'){
  #   final_cdst <- cohort_codeset_label_omop(cohort_tbl = stnd,
  #                                           codeset_meta = codeset)
  #   }else if(omop_or_pcornet == 'pcornet'){
  #     final_cdst <- cohort_codeset_label_pcnt(cohort_tbl = stnd,
  #                                             codeset_meta = codeset)
  #   }
  # }

  final <- stnd %>%
    left_join(final_age) #%>%
    #left_join(final_cdst)

  return(final)

}


#' Build birth_date Column
#'
#' This function, which is specific to the OMOP CDM implementation, will create
#' an aggregate birth_date column using year_of_birth, month_of_birth, and
#' day_of_birth
#'
#' @param cohort table with cohort members; must at least have person_id
#' @param person_tbl CDM person table; must at least have person_id, year_of_birth,
#' month_of_birth, and day_of_birth
#'
#' @return a table with a new birth_date column that is a combination of year, month, and
#' day of birth; improves ability to compute ages
#'
#' @examples
#' \dontrun{
#' sample_cohort <- dplyr::tibble(site = c('Site A', 'Site B'),
#'                                person_id = c(1, 2),
#'                                start_date = c(as.Date('2012-01-01'),
#'                                               as.Date('2014-02-01)),
#'                                end_date = c(as.Date('2015-01-01'),
#'                                             as.Date('2020-02-01)))
#'
#' build_birth_date(cohort = copy_to_new(df = sample_cohort),
#'                  person_tbl = cdm_tbl('person'))
#'
#' }
#'
#' @importFrom lubridate make_date
#' @export
#'
build_birth_date <- function(cohort,
                             person_tbl){

  person_tbl %>%
    select(person_id, year_of_birth, month_of_birth, day_of_birth) %>%
    inner_join(cohort) %>%
    collect() %>%
    mutate(month_of_birth = ifelse(is.na(month_of_birth), 1, month_of_birth),
           day_of_birth = ifelse(is.na(day_of_birth), 1, day_of_birth)) %>%
    mutate(birth_date = lubridate::make_date(year = year_of_birth, month = month_of_birth,
                                             day = day_of_birth))

}

#' Calculate Date Differences in Multiple SQL Backends
#'
#' Function to get sql code for number of days between date1 and date2.
#' Adapted for sql dialects for Postgres and MS SQL.
#'
#' Should always be wrapped by sql()
#' @param date_col_1 Date col 1
#' @param date_col_2 Date col 2
#' @param db connection type object. Defaulted to config('db_src') for standard framework
#' Functionality added for Postgres, MS SQL and Snowflake
#'
#' @return an integer representing the difference (in days) between the two provided
#' dates
#'
calc_days_between_dates <-
  function(date_col_1, date_col_2, db = get_argos_default()$config("db_src")) {

    if (class(db)[1] == "Microsoft SQL Server") {
      sql_code <-
        paste0("DATEDIFF(day, ", date_col_1, ", ", date_col_2, ")")
    } else if (class(db)[1] == "Snowflake") {
      sql_code <-
        paste0("DATEDIFF(day, ",'"',date_col_1,'"',", ",'"',date_col_2,'"',")")
    } else if (class(db)[1] == "Oracle") {
      sql_code <- paste0('("', date_col_2, '" - "', date_col_1, '")')
    } else if (class(db)[1] == "src_BigQueryConnection") {
      sql_code <- paste0("DATE_DIFF(",date_col_1,", ",date_col_2,", DAY)")
    } else if(class(db) %in% 'SQLiteConnection'){
      sql_code <-
        paste0("julianday(", date_col_2, ") - julianday(", date_col_1, ")")
    }else if(class(db) %in% 'PrestoConnection'){
      sql_code <-
        paste0("date_diff(day, ", date_col_1, ", ", date_col_2, ")")
    } else {
      sql_code <-
        paste0(date_col_2, " - ", date_col_1)
    }
    return(sql_code)
  }
