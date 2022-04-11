.onLoad <- function(libname, pkgname) {
  myenv <- asNamespace(pkgname)
  assign("make_table_mem", memoise::memoise(make_table), envir = myenv)

  # Whitelist these files to auto-load
  autoload_files <- c("dash_data", "dash_data_updated", "sa42016", "employment_regions2015")
  lapply(autoload_files, function(var_name) {
    fn <- system.file(sprintf("extdata/%s.qs", var_name), package = pkgname)
    assign(var_name, qs::qread(fn), envir = myenv)
    NULL # don't collect unused result
  })
}
