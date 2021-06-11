
test_that("All viz_* functions are included in a page_* file", {
  viz_funcs <- ls("package:djprlabourdash", pattern = "viz_")
  ids <- gsub("viz_", "", viz_funcs)


  page_funcs <- ls("package:djprlabourdash", pattern = "page_")

  name_to_eval <- function(func_name_as_string) {
    suppressMessages(
      eval(str2lang(paste0(func_name_as_string, "()")))
    )
  }

  pages <- list()
  for (f in page_funcs) {
    pages[[f]] <- name_to_eval(f)
  }

  pages <- unlist(pages)

  ids_in_pages <- sapply(
    ids,
    function(x) any(grepl(x, pages))
  )

  if (length(ids_in_pages[!ids_in_pages]) > 0) {
    print(paste0(
      "The function ",
      viz_funcs[!ids_in_pages],
      "() is not included on any dashboard page."
    ))
  }

  expect_true(all(ids_in_pages))
})
