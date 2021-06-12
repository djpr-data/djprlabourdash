.onLoad <- function(libname, pkgname) {
  shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "djpr-jobs-cache")))
}
