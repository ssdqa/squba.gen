#' Age at Cohort Entry -- OMOP
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param person_tbl the CDM person table
#' @param age_groups a table where the user defines the minimum and maximum
#'                   age allowed for a group and provides a string name for the group
#'
#' @return `cohort_tbl` with the age at cohort entry and age group for each patient
#'
#' @keywords internal
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
#' @keywords internal
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


#' Prepare Cohort
#'
#' For the cohort table input into any of the *_process functions, this function
#' will prepare that cohort for the rest of the analysis. This includes computing
#' follow-up based on start and end dates and computing age at cohort entry categorized
#' into user-provided groups.
#'
#' @param cohort_tbl *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer / character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#' @param age_groups *tabular input* || defaults to `NULL`
#'
#'   If you would like to stratify the results by age group, create a table or
#'   CSV file with the following columns and use it as input to this parameter:
#'
#'   - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#'   - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#'   - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'   If you would *not* like to stratify by age group, leave as `NULL`
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#' @return
#'   This function will return the same input cohort table with the addition of
#'   the column `fu`, which includes the length of patient follow-up based on the
#'   provided start & end dates. Optionally, if age_groups is not NULL, the output
#'   will also include `age_ce` (patient age at cohort entry) and `age_grp`
#'   (the user-provided age group category)
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
                           omop_or_pcornet) {


  if(any(class(cohort_tbl) %in% 'tbl_sql')){
    ct <- cohort_tbl
  }else{
    ct <- copy_to_new(df = cohort_tbl)
  }

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

  final <- stnd %>%
    left_join(final_age)

  return(final)

}


#' Build birth_date Column
#'
#' This function, which is specific to the OMOP CDM implementation, will create
#' an aggregate birth_date column using `year_of_birth`, `month_of_birth`, and
#' `day_of_birth`
#'
#' @param cohort *tabular input* || **required**
#'
#'   A table with cohort members, which contains at least a `person_id` column
#'
#' @param person_tbl *tabular input* || **required**
#'
#'   The CDM `person` table or another table that contains at least `person_id`,
#'   `year_of_birth`, `month_of_birth`, and `day_of_birth`
#'
#' @return
#'   This function will return a table with a new birth_date column that is a
#'   combination of year, month, and day of birth. This is intended to facilitate
#'   computations using date of birth, like computing age.
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
#' Adapted for sql dialects for Postgres, MSSQL, Snowflake, Oracle, BigQuery,
#' Presto, & Spark.
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
#' @keywords internal
#'
calc_days_between_dates <-
  function(date_col_1, date_col_2, db = get_argos_default()$config("db_src")) {

    if (class(db)[1] == "Microsoft SQL Server") {
      sql_code <-
        paste0("DATEDIFF(day, ", date_col_1, ", ", date_col_2, ")")
    } else if (class(db)[1] == "Snowflake") {
      sql_code <-
        paste0(
          "DATEDIFF(day, ",
          '"',
          date_col_1,
          '"',
          ", ",
          '"',
          date_col_2,
          '"',
          ")"
        )
    } else if (class(db)[1] == "Oracle") {
      sql_code <- paste0(
        'TRUNC("',
        date_col_2,
        '") - TRUNC("',
        date_col_1,
        '")'
      )
    } else if (class(db)[1] == "src_BigQueryConnection") {
      sql_code <- paste0("DATE_DIFF(", date_col_1, ", ", date_col_2, ", DAY)")
    } else if (class(db) %in% 'SQLiteConnection') {
      sql_code <-
        paste0("julianday(", date_col_2, ") - julianday(", date_col_1, ")")
    } else if (class(db) %in% 'PrestoConnection') {
      sql_code <-
        paste0("date_diff('day', ", date_col_1, ", ", date_col_2, ")")
    } else if (class(db)[1] %in% 'Spark SQL') {
      sql_code <-
        paste0("datediff(", date_col_2, ",", date_col_1, ")")
    } else {
      sql_code <-
        paste0(date_col_2, "::date", " - ", date_col_1, "::date")
    }
    return(sql_code)
  }
