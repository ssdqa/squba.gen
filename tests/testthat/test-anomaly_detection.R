
test_that('compute_dist_mean_median', {

  sample_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd', 'scd'),
                                'site' = c('Site A', 'Site A', 'Site B', 'Site B'),
                                'count' = c(15, 24, 100, 93))

  expect_no_error(compute_dist_mean_median(tbl = sample_input,
                                           grp_vars = 'variable',
                                           var_col = 'count',
                                           num_sd = 1,
                                           num_mad = 1))

})


test_that('euclidean', {

  sample_ms_la_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd', 'scd',
                                                     'scd', 'scd', 'scd', 'scd',
                                                     'scd', 'scd', 'scd',
                                                     'scd', 'scd', 'scd'),
                                      'site' = c('Site A', 'Site A', 'Site A', 'Site A',
                                                 'Site A', 'Site A', 'Site A',
                                                 'Site B', 'Site B', 'Site B', 'Site B',
                                                 'Site B', 'Site B', 'Site B'),
                                      'count' = c(15, 24, 100, 93, 47, 65, 33, 92, 153, 122, 5,
                                                  99, 10, 30),
                                      'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01',
                                                       '2022-01-01', '2023-01-01', '2024-01-01',
                                                       '2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01',
                                                       '2022-01-01', '2023-01-01', '2024-01-01'),
                                      'time_increment' = c('year','year','year','year','year','year',
                                                           'year','year','year','year','year','year',
                                                           'year','year'))

  expect_no_error(ms_anom_euclidean(fot_input_tbl = sample_ms_la_input %>%
                                      mutate(time_start = as.Date(time_start)),
                                    grp_vars = c('site', 'variable'),
                                    var_col = 'count'))


})


test_that('anomalization', {

  sample_ss_la_input <- dplyr::tibble('variable' = c('scd', 'scd', 'scd', 'scd',
                                                     'scd', 'scd', 'scd', 'scd',
                                                     'scd', 'scd', 'scd',
                                                     'scd', 'scd', 'scd'),
                                      'site' = c('Site A', 'Site A', 'Site A', 'Site A',
                                                 'Site A', 'Site A', 'Site A',
                                                 'Site A', 'Site A', 'Site A', 'Site A',
                                                 'Site A', 'Site A', 'Site A'),
                                      'count' = c(15, 24, 100, 93, 47, 65, 33, 92, 153, 122, 5,
                                                  99, 10, 30),
                                      'time_start' = c('2018-01-01', '2018-02-01', '2018-03-01', '2018-04-01',
                                                       '2018-05-01', '2018-06-01', '2018-07-01',
                                                       '2018-08-01', '2018-09-01', '2018-10-01', '2018-11-01',
                                                       '2018-12-01', '2019-01-01', '2019-02-01'),
                                      'time_increment' = c('month','month','month','month','month','month',
                                                           'month','month','month','month','month','month',
                                                           'month','month'))

  expect_no_error(anomalize_ss_anom_la(fot_input_tbl = sample_ss_la_input %>%
                                         mutate(time_start = as.Date(time_start)),
                                       grp_vars = c('site', 'variable'),
                                       time_var = 'time_start',
                                       var_col = 'count'))

  expect_no_error(anomalize_ss_anom_la(fot_input_tbl = sample_ss_la_input %>%
                                         mutate(time_start = as.Date(time_start),
                                                time_increment = 'year'),
                                       grp_vars = c('site', 'variable'),
                                       time_var = 'time_start',
                                       var_col = 'count'))

})


test_that('ms_anom_cs', {


  sample_ms_input <- dplyr::tibble('site' = c('Site A', 'Site A', 'Site A', 'Site A',
                                              'Site B', 'Site B', 'Site B', 'Site B'),
                                   'variable' = c('dx', 'dx', 'drug', 'drug',
                                                  'dx', 'dx', 'drug', 'drug'),
                                   'count' = c(100, 140, 39, 42, 137, 111, 12, 15),
                                   'total_var' = c(1000, 1000, 200, 200, 1500, 1500,
                                                   100, 100))

  expect_no_error(compute_dist_anomalies(df_tbl = sample_ms_input,
                                         grp_vars = c('variable'),
                                         var_col = 'count',
                                         denom_cols = 'total_var'))

  anom_test <- compute_dist_anomalies(df_tbl = sample_ms_input,
                                      grp_vars = c('variable'),
                                      var_col = 'count',
                                      denom_cols = 'total_var')

  expect_no_error(detect_outliers(df_tbl = anom_test,
                                  column_analysis = 'count',
                                  column_variable = 'variable'))

  expect_warning(detect_outliers(df_tbl = anom_test %>%
                                   mutate(analysis_eligible = 'no'),
                                 column_analysis = 'count',
                                 column_variable = 'variable'))

})


test_that('jaccard', {

  sample_jaccard_input <- dplyr::tibble('person_id' = c(1,1,2,2,3,4,5,5),
                                        'variable' = c('dx', 'drug', 'dx', 'drug', 'drug', 'dx',
                                                       'drug', 'dx'))

  expect_no_error(compute_jaccard(jaccard_input_tbl = sample_jaccard_input,
                                  var_col = 'variable',
                                  omop_or_pcornet = 'omop'))

  expect_no_error(compute_jaccard(jaccard_input_tbl = sample_jaccard_input %>%
                                    mutate(patid = person_id),
                                  var_col = 'variable',
                                  omop_or_pcornet = 'pcornet'))

})
