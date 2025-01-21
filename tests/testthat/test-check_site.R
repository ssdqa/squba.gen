
test_that("Single Site + 1 Site", {

  cohort <- dplyr::tibble(site = c('Site A', 'Site A', 'Site A'),
                          person_id = c(1, 2, 3))

  expect_no_error(check_site_type(cohort = cohort,
                                  multi_or_single_site = 'single'))

  cohort2 <- check_site_type(cohort = cohort,
                             multi_or_single_site = 'single')[[1]]

  expect_equal(colnames(cohort2), c('site', 'person_id'))

})


test_that("Single Site + Multiple Sites", {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  expect_no_error(check_site_type(cohort = cohort,
                                  multi_or_single_site = 'single'))

  cohort2 <- check_site_type(cohort = cohort,
                             multi_or_single_site = 'single')[[1]]

  expect_equal(colnames(cohort2), c('site', 'person_id', 'site_summ'))

})

test_that("Multi Site + 1 Site", {

  cohort <- dplyr::tibble(site = c('Site A', 'Site A', 'Site A'),
                          person_id = c(1, 2, 3))

  expect_error(check_site_type(cohort = cohort,
                               multi_or_single_site = 'multi'))

})

test_that("Multi Site + Multiple Sites", {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  expect_no_error(check_site_type(cohort = cohort,
                                  multi_or_single_site = 'multi'))

  cohort2 <- check_site_type(cohort = cohort,
                             multi_or_single_site = 'multi')[[1]]

  expect_equal(colnames(cohort2), c('site', 'person_id'))

})


test_that("No Site Col Error", {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  expect_error(check_site_type(cohort = cohort %>% select(-site),
                               multi_or_single_site = 'multi'))

})

test_that("Invalid Arg Error", {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  expect_error(check_site_type(cohort = cohort,
                               multi_or_single_site = 'test'))

})


test_that('Replace site_summ with site col', {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  cohort2 <- check_site_type(cohort = cohort,
                             multi_or_single_site = 'single')[[1]]

  expect_no_error(replace_site_col(cohort2))

  cohort3 <- replace_site_col(cohort2)

  expect_equal(colnames(cohort3), c('site', 'person_id'))

})


test_that('Replace site_summ without site col', {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  cohort2 <- check_site_type(cohort = cohort,
                             multi_or_single_site = 'single')[[1]] %>%
    select(-site)

  expect_no_error(replace_site_col(cohort2))

  cohort3 <- replace_site_col(cohort2)

  expect_equal(colnames(cohort3), c('person_id', 'site'))

})


test_that('Replace site with neither', {

  cohort <- dplyr::tibble(site = c('Site A', 'Site B', 'Site C'),
                          person_id = c(1, 2, 3))

  cohort2 <- check_site_type(cohort = cohort,
                             multi_or_single_site = 'single')[[1]] %>%
    select(-c(site, site_summ))

  expect_no_error(replace_site_col(cohort2))

  cohort3 <- replace_site_col(cohort2)

  expect_equal(colnames(cohort3), c('person_id'))

})
