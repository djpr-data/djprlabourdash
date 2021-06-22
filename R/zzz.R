.onLoad <- function(libname, pkgname) {
  # shinyOptions(
  #   cache = cachem::cache_disk(
  #     dir = file.path(dirname(tempdir()), "djpr-jobs-dash-cache")
  #     )
  # )

  make_reactable_mem <<- memoise::memoise(make_reactable)
}
