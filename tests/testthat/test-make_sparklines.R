test_that("make_sparklines() makes sparklines", {
  econ_sparks <- make_sparklines(df = ggplot2::economics_long,
                  group_var = variable)

  expect_true(
    lapply(econ_sparks, inherits, "gg") %>%
      as.logical() %>%
      all()
  )

  expect_length(econ_sparks, 5)
  expect_identical(names(econ_sparks), unique(ggplot2::economics_long$variable))

  # Check that make_sparklines() respects the ordering of the grouping variable
  reordered_econ_df <- ggplot2::economics_long %>%
    dplyr::arrange(desc(variable))

  reordered_econ_sparks <- reordered_econ_df %>%
    make_sparklines(group_var = variable)

  expect_false(identical(names(reordered_econ_sparks), names(econ_sparks)))
  expect_identical(names(reordered_econ_sparks), c("unemploy",
                                                   "uempmed",
                                                   "psavert",
                                                   "pop",
                                                   "pce"))

  # make_sparklines() should fail when supplied a non-date var as date_var
  expect_error(make_sparklines(df = ggplot2::mpg,
                               group_var = model,
                               date_var = year))

  mpg_sparks <- ggplot2::mpg %>%
    mutate(year = as.Date(paste0(year, "-01-01"))) %>%
    make_sparklines(date_var = year,
                    value_var = cty,
                    group_var = model)

  expect_length(mpg_sparks, length(unique(ggplot2::mpg$model)))
  expect_identical(names(mpg_sparks), unique(ggplot2::mpg$model))
  expect_true(
    lapply(mpg_sparks, inherits, "gg") %>%
      as.logical() %>%
      all()
  )

})
