
test_that('join to vocabulary table', {

  vocab_concepts <- dplyr::tibble('concept_id' = c(1, 2, 3, 4),
                                  'concept_name' = c('test1', 'test2', 'test3', 'test4'),
                                  'vocabulary_id' = c('SNOMED', 'ICD10', 'ICD9', 'CPT4'))

  input_tbl <- dplyr::tibble('concept_id' = c(1, 2, 3, 4),
                             'ct_concept' = c(100, 200, 300, 400))

  expect_no_error(join_to_vocabulary(tbl = input_tbl,
                                     vocab_tbl = vocab_concepts,
                                     col = 'concept_id',
                                     vocab_col = 'concept_id'))

  expect_no_error(join_to_vocabulary(tbl = input_tbl,
                                     vocab_tbl = NULL,
                                     col = 'concept_id',
                                     vocab_col = 'concept_id'))


})


test_that('paramater summarization', {

  expect_no_error(param_summ(check_string = 'evp',
                             multi_or_single_site = 'single',
                             anomaly_or_exploratory = 'exploratory',
                             time = FALSE))

  expect_no_error(param_summ(check_string = 'evp',
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             time = TRUE))


})


test_that('generate reference table', {

  input_tbl_notime <- dplyr::tibble('concept_id' = c(1, 2, 3, 4),
                                    'concept_name' = c('test1', 'test2', 'test3', 'test4'),
                                    'ct_concept' = c(100, 200, 300, 400),
                                    'site' = c('Site A', 'Site A', 'Site A', 'Site A'))

  input_tbl_time <- dplyr::tibble('concept_id' = c(1, 2, 3, 4, 1, 2, 3, 4),
                                  'time_start' = c('2012-01-01', '2012-01-01',
                                                   '2012-01-01', '2012-01-01',
                                                   '2013-01-01', '2013-01-01',
                                                   '2013-01-01', '2013-01-01'),
                                  'time_increment' = c('year','year','year','year',
                                                       'year','year','year','year'),
                                  'concept_name' = c('test1', 'test2', 'test3', 'test4',
                                                     'test1', 'test2', 'test3', 'test4'),
                                  'ct_concept' = c(100, 200, 300, 400,
                                                   200, 300, 400, 500),
                                  'site' = c('Site A', 'Site A', 'Site A', 'Site A',
                                             'Site A', 'Site A', 'Site A', 'Site A'))

  expect_no_error(generate_ref_table(tbl = input_tbl_notime,
                                     id_col = 'concept_id',
                                     name_col = 'concept_name',
                                     denom = 'ct_concept',
                                     time = FALSE))

  expect_no_error(generate_ref_table(tbl = input_tbl_time,
                                     id_col = 'concept_id',
                                     name_col = 'concept_name',
                                     denom = 'ct_concept',
                                     time = TRUE))


})


test_that('interactive ssdqa output', {

  grph1 <- mtcars %>%
    ggplot(aes(x = as.character(gear), y = mean(wt),
               fill = mpg, tooltip = mpg)) +
    ggiraph::geom_col_interactive()

  expect_message(make_interactive_ssdqa(grph1))

  grph1[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

  expect_no_error(make_interactive_ssdqa(grph1))

  grph2 <- mtcars %>%
    ggplot(aes(x = as.character(gear), y = mean(wt),
               fill = mpg, tooltip = mpg)) +
    geom_col()

  grph2[['metadata']] <- tibble('pkg_backend' = 'plotly',
                               'tooltip' = FALSE)

  expect_no_error(make_interactive_ssdqa(grph2))

  grph3 <- mtcars %>%
    ggplot(aes(x = mpg, y = wt,
               color = cyl, shape = as.character(cyl))) +
    geom_point()

  grph3[['metadata']] <- tibble('pkg_backend' = 'plotly_ssc',
                                'tooltip' = FALSE)

  expect_no_error(make_interactive_ssdqa(grph3))

})
