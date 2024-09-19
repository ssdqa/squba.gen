
# test_that('Session can be established with DBI conn object', {
#   expect_success(initialize_dq_session(session_name = 'test',
#                                        db_conn = ,
#                                        working_directory = getwd(),
#                                        file_directory = 'specs',
#                                        is_json = FALSE,
#                                        cdm_schema = 'test_schema',
#                                        results_schema = 'test_results',
#                                        results_tag = NULL))
# })
#
#
# test_that('Session can be established with json', {
#   expect_success(initialize_dq_session(session_name = 'test',
#                                        db_conn = ,
#                                        is_json = TRUE,
#                                        working_directory = getwd(),
#                                        file_directory = 'specs',
#                                        cdm_schema = 'test_schema',
#                                        results_schema = 'test_results',
#                                        results_tag = NULL))
# })
#
#
# test_that('CDM table can be read', {
#   tbl <- cdm_tbl('person')
#   expect_s3_class(tbl, 'tbl')
# })
