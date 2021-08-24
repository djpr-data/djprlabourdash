devtools::load_all()

# dash_data <<- load_dash_data()

# Evaluate each viz function -----
viz_funcs <- ls("package:djprlabourdash", pattern = "viz_")

name_to_eval <- function(func_name_as_string) {
  suppressMessages(
    eval(str2lang(paste0(func_name_as_string, "()")))
  )
}

plots <- tibble()
for (f in viz_funcs) {
  x <- bench::mark(
    # print(
    name_to_eval(f)
    # )
    , time_unit = "s",
    min_iterations = 10)
  x <- as_tibble(x)
  x$expression <- f
  x <- dplyr::select(x, expression, median, `itr/sec`)
  plots <- bind_rows(plots, x)
}

plots %>%
  arrange(-median)


View(plots)


# table_funcs <- ls("package:djprlabourdash", pattern = "table_")
# tables <- tibble()
# for (f in table_funcs) {
#   x <- bench::mark(print(name_to_eval(f)), time_unit = "s")
#   x <- as_tibble(x)
#   x$expression <- f
#   x <- dplyr::select(x, expression, median, `itr/sec`)
#   tables <- bind_rows(tables, x)
# }
