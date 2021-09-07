.onLoad <- function(libname, pkgname) {
  jobs_dash_cache <- cachem::cache_disk(
    # dir = file.path(dirname(tempdir()), "djpr-jobs-dash-cache")
    dir = file.path(".", "app-cache")
  )

  shinyOptions(
    cache = jobs_dash_cache
  )

  if (requireNamespace("memoise", quietly = TRUE)) {
    make_table_mem <<- memoise::memoise(make_table,
      cache = cachem::cache_disk()
    )
  } else {
    make_table_mem <<- make_table
  }
}
