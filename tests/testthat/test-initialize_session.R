
test_that('Session can be established', {

  conn <- mk_testdb_omop()

  expect_no_error(initialize_dq_session(session_name = 'test',
                                        db_conn = conn,
                                        working_directory = getwd(),
                                        file_subdirectory = 'specs',
                                        is_json = FALSE,
                                        cdm_schema = 'test_schema',
                                        results_schema = 'test_results',
                                        results_tag = NULL))
})

