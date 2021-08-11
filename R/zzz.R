.onLoad <- function(libname, pkgname) {
  shinyOptions(
    cache = cachem::cache_disk(
      dir = file.path(dirname(tempdir()), "djpr-jobs-dash-cache")
    )
  )

  if (suppressWarnings(require(memoise))) {
    make_table_mem <<- memoise::memoise(make_table)
  } else {
    make_table_mem <<- make_table
  }
}
