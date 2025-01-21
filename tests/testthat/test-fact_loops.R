

test_that('loop through visits omop', {

  mock_function <- function(cohort,
                            input_tbl){


    test <- input_tbl %>%
      inner_join(cohort) %>%
      group_by(site) %>%
      summarise(n_row = n())

    print(test$n_row)

    return(test)

  }

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'loop_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testdata',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  visit_types <- patientfacts::pf_visit_file_omop
  domains <- patientfacts::pf_domain_file

  expect_no_error(
    loop_through_visits(cohort_tbl = cohort,
                       omop_or_pcornet = 'omop',
                       check_func = function(cht, t){
                                   mock_function(cohort = cht,
                                                 input_tbl = t)},
                       site_col = 'site',
                       time = FALSE,
                       visit_type_tbl = visit_types,
                       visit_list = c('outpatient'),
                       visit_tbl = cdm_tbl('visit_occurrence'),
                       site_list = c('synth2'),
                       grouped_list = c('person_id','start_date','end_date',
                                       'site'),
                       domain_tbl = domains %>% dplyr::filter(domain == 'diagnoses'))
    )

  expect_no_error(
    loop_through_visits(cohort_tbl = cohort,
                        omop_or_pcornet = 'omop',
                        check_func = function(cht, t){
                          mock_function(cohort = cht,
                                        input_tbl = t)},
                        site_col = 'site',
                        time = FALSE,
                        visit_type_tbl = visit_types %>% mutate(visit_detail_concept_id = visit_concept_id),
                        visit_list = c('outpatient'),
                        visit_tbl = cdm_tbl('visit_occurrence') %>%
                          mutate(visit_detail_id = visit_occurrence_id,
                                 visit_detail_start_date = visit_start_date,
                                 visit_detail_concept_id = visit_concept_id),
                        site_list = c('synth2'),
                        grouped_list = c('person_id','start_date','end_date',
                                         'site'),
                        domain_tbl = domains %>% dplyr::filter(domain == 'diagnoses'))
  )

  expect_error(
    loop_through_visits(cohort_tbl = cohort,
                        omop_or_pcornet = 'test',
                        check_func = function(cht, t){
                          mock_function(cohort = cht,
                                        input_tbl = t)},
                        site_col = 'site',
                        time = FALSE,
                        visit_type_tbl = visit_types,
                        visit_list = c('outpatient'),
                        visit_tbl = cdm_tbl('visit_occurrence'),
                        site_list = c('synth2'),
                        grouped_list = c('person_id','start_date','end_date',
                                         'site'),
                        domain_tbl = domains %>% dplyr::filter(domain == 'diagnoses'))
  )



})


test_that('loop through visits pcornet', {

  mock_function <- function(cohort,
                            input_tbl){


    test <- input_tbl %>%
      inner_join(cohort) %>%
      group_by(site) %>%
      summarise(n_row = n())

    print(test$n_row)

    return(test)

  }

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'loop_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testdata',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'),
           patid = person_id)

  visit_types <- patientfacts::pf_visit_file_pcornet %>%
    dplyr::add_row('enc_type' = '9202', 'visit_type' = 'outpatient')
  domains <- patientfacts::pf_domain_file

  expect_no_error(
    loop_through_visits(cohort_tbl = cohort,
                        omop_or_pcornet = 'pcornet',
                        check_func = function(cht, t){
                          mock_function(cohort = cht,
                                        input_tbl = t)},
                        site_col = 'site',
                        time = FALSE,
                        visit_type_tbl = visit_types,
                        visit_list = c('outpatient'),
                        visit_tbl = cdm_tbl('visit_occurrence') %>%
                          mutate(patid = person_id,
                                 enc_type = visit_concept_id,
                                 admit_date = visit_start_date,
                                 encounterid = visit_occurrence_id),
                        site_list = c('synth2'),
                        grouped_list = c('person_id','start_date','end_date',
                                         'site'),
                        domain_tbl = domains %>% dplyr::filter(domain == 'diagnoses'))
  )



})



test_that('fot loop', {

  mock_function <- function(cohort,
                            input_tbl){


    test <- input_tbl %>%
      inner_join(cohort) %>%
      group_by(site, time_start) %>%
      summarise(n_row = n())

    print(test$n_row)

    return(test)

  }

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'loop_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testdata',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'),
           patid = person_id)


  expect_no_error(

    compute_fot(cohort = cohort,
                check_func = function(dat){
                  mock_function(cohort = dat,
                                input_tbl = cdm_tbl('condition_occurrence'))
                },
                site_col = 'site',
                reduce_id = NULL,
                site_list = c('synth1', 'synth2'))

  )

})
