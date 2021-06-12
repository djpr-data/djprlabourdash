.onLoad <- function(libname, pkgname) {
  shinyOptions(
    cache = cachem::cache_disk(
      dir = file.path(dirname(tempdir()), "djpr-jobs-cache"),
      max_age =
      )
  )
}
