## Create internal data
pkgload::load_all()

# Load absmapsdata object(s) to avoid dependency
if (requireNamespace("absmapsdata", quietly = TRUE)) {
  sa42016 <- absmapsdata::sa42016
  employment_regions2015 <- absmapsdata::employment_regions2015
} else {
  sa42016 <- sa42016
  employment_regions2015 <- employment_regions2015
}


# Load data from djprdashdata if it has been updated
remote_updated <- check_remote_updated()
# Exists check is because `dash_data_updated` will not exist if `sysdata.rda`
# has been cleared.
if (!exists("dash_data_updated") || (dash_data_updated != remote_updated)) {
  dash_data <- load_dash_data()
  stopifnot(inherits(dash_data, "tbl_df"))
  stopifnot(nrow(dash_data) > 800)
  dash_data_updated <- remote_updated
}

usethis::use_data(sa42016,
  employment_regions2015,
  dash_data_updated,
  dash_data,
  internal = TRUE, overwrite = TRUE
)
