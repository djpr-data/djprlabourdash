.onLoad <- function(libname, pkgname) {
  # shinyOptions(
  #   cache = cachem::cache_disk(
  #     dir = file.path(dirname(tempdir()), "djpr-jobs-dash-cache")
  #     )
  # )

  if (requireNamespace("memoise", quietly = TRUE)) {
    make_reactable_mem <<- memoise::memoise(make_reactable)
  } else {
    make_reactable_mem <<- make_reactable
  }

}
