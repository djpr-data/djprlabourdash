## Create internal data
pkgload::load_all()

dir.create("inst/extdata", showWarnings = F, recursive = T)

if (requireNamespace("absmapsdata", quietly = T)) {
  # These don't change, but are fast enough to keep in the main script
  qs::qsave(absmapsdata::sa42016, "inst/extdata/sa42016.qs")
  qs::qsave(absmapsdata::employment_regions2015, "inst/extdata/employment_regions2015.qs")
}

dash_data <- get_dash_data(verbose = T) %>%
                filter(table_no != 'salm')
stopifnot(inherits(dash_data, "tbl_df"))
stopifnot(nrow(dash_data) > 800)
dash_data_updated <- attr(dash_data, "date_updated")

qs::qsave(dash_data, "inst/extdata/dash_data.qs")
qs::qsave(dash_data_updated, "inst/extdata/dash_data_updated.qs")
