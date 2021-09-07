test_that("all viz_*() functions at least produce a plot", {

  # Load dash data -----
  # Use `<<-` so that dash_data is available to viz functions
  myenv <- as.environment(1)

  x <- load_dash_data()

  x <- x %>%
    tidyr::unnest(cols = .data$data) %>%
    # Restrict date range so that chart doesn't change as new data comes in
    dplyr::filter(.data$date <= as.Date("2021-03-01"))

  assign("dash_data",
         x,
         envir = myenv)

  # Re-nest
  dash_data <<- dash_data %>%
    group_by(select(
      cur_data_all(),
      -one_of(c(
        "date",
        "value",
        "series",
        "series_type",
        "table_no",
        "data_type",
        "frequency",
        "unit"
      ))
    )) %>%
    dplyr::group_nest()

  # Evaluate each viz function -----
  viz_funcs <- ls("package:djprlabourdash", pattern = "viz_")

  name_to_eval <- function(func_name_as_string) {
    suppressMessages(
      eval(str2lang(paste0(func_name_as_string, "()")),
           envir = myenv)
    )
  }

  plots <- list()
  for (f in viz_funcs) {
    plots[[f]] <- name_to_eval(f)
  }

  # Test every result is a ggplot -----

  lapply(
    plots,
    function(x) expect_s3_class(x, "ggplot")
  )

  for (i in seq_along(viz_funcs)) {
    vdiffr::expect_doppelganger(
      title = viz_funcs[i],
      fig = plots[[i]]
    )
  }
})
