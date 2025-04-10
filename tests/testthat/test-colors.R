
test_that('fill colors', {

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = as.character(gear), y = mean(wt),
                 fill = as.character(gear))) +
      geom_col() +
      scale_fill_squba()
  )

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = as.character(gear), y = mean(wt),
                 fill = mpg)) +
      geom_col() +
      scale_fill_squba(discrete = FALSE, reverse = TRUE)
  )
})


test_that('color colors', {

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = mpg, y = wt,
                 color = cyl)) +
      geom_point() +
      scale_color_squba(discrete = FALSE)
  )

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = mpg, y = wt,
                 color = as.character(cyl))) +
      geom_point() +
      scale_color_squba()
  )

})


test_that('extract color', {

  expect_no_error(extract_color())

  expect_no_error(extract_color("rust"))

})
