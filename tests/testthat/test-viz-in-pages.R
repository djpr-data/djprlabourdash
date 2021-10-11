
test_that("All viz_* functions are included in a page_* file", {
  viz_funcs <- ls(getNamespace("djprlabourdash"), pattern = "viz_")
  ids <- gsub("viz_", "", viz_funcs)

  page_funcs <- ls(getNamespace("djprlabourdash"), pattern = "page_")

  pages <- list()
  for (f in page_funcs) {
    pages[[f]] <- get(f)()
  }

  pages <- unlist(pages)

  ids_in_pages <- sapply(
    ids,
    function(x) any(grepl(x, pages))
  )

  if (!all(ids_in_pages)) {
    print(paste0(
      "The function ",
      viz_funcs[!ids_in_pages],
      "() is not included on any dashboard page."
    ))
  }

  expect_true(all(ids_in_pages))
})
