
test_that('fill colors', {

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = as.character(gear), y = mean(wt),
                 fill = as.character(gear))) +
      geom_col() +
      scale_fill_ssdqa()
  )

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = as.character(gear), y = mean(wt),
                 fill = mpg)) +
      geom_col() +
      scale_fill_ssdqa(discrete = FALSE, reverse = TRUE)
  )
})


test_that('color colors', {

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = mpg, y = wt,
                 color = cyl)) +
      geom_point() +
      scale_color_ssdqa(discrete = FALSE)
  )

  expect_no_error(
    mtcars %>%
      ggplot(aes(x = mpg, y = wt,
                 color = as.character(cyl))) +
      geom_point() +
      scale_color_ssdqa()
  )

})


test_that('extract color', {

  expect_no_error(extract_color())

  expect_no_error(extract_color("rust"))

})
