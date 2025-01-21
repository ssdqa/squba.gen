
test_that('cohort preparation with NULLs', {

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'test',
                        db_conn = conn,
                        working_directory = getwd(),
                        file_subdirectory = 'testdata',
                        is_json = FALSE,
                        cdm_schema = NA)

  cht <- dplyr::tibble(site = 'Site A',
                       person_id = 1,
                       start_date = as.Date('2012-01-01'),
                       end_date = as.Date('2020-01-01'))

  expect_no_error(prepare_cohort(cohort_tbl = cht,
                                 omop_or_pcornet = 'omop'))

  expect_no_error(prepare_cohort(cohort_tbl = cht,
                                 omop_or_pcornet = 'pcornet'))

})


test_that('cohort preparation with ages', {

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'test',
                        db_conn = conn,
                        working_directory = getwd(),
                        file_subdirectory = 'testdata',
                        is_json = FALSE,
                        cdm_schema = NA)

  cht <- dplyr::tibble(site = 'Site A',
                       person_id = 1,
                       start_date = as.Date('2012-01-01'),
                       end_date = as.Date('2020-01-01'))

  age_grps <- dplyr::tibble(min_age = 0,
                            max_age = 100,
                            group = 'everyone')

  expect_no_error(prepare_cohort(cohort_tbl = cht,
                                 age_groups = age_grps,
                                 omop_or_pcornet = 'omop'))

  expect_no_error(prepare_cohort(cohort_tbl = cht %>% dplyr::rename('patid' = person_id),
                                 age_groups = age_grps,
                                 omop_or_pcornet = 'pcornet'))

})


test_that('OMOP make birth_date column', {


  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'test',
                        db_conn = conn,
                        working_directory = getwd(),
                        file_subdirectory = 'testdata',
                        is_json = FALSE,
                        cdm_schema = NA)

  cht <- dplyr::tibble(site = 'Site A',
                       person_id = 1,
                       start_date = as.Date('2012-01-01'),
                       end_date = as.Date('2020-01-01'))

  expect_no_error(build_birth_date(cohort = copy_to_new(df = cht),
                                   person_tbl = cdm_tbl('person')))

})
