.onLoad <- function(libname, pkgname) {

  if (requireNamespace("memoise", quietly = TRUE)) {
    make_table_mem <<- memoise::memoise(make_table,
      cache = cachem::cache_disk()
    )
  } else {
    make_table_mem <<- make_table
  }
}
