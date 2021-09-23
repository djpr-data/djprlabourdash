## Create internal data
pkgload::load_all()

# Load absmapsdata object(s) to avoid dependency
if (requireNamespace("absmapsdata", quietly = TRUE)) {
  sa42016 <- absmapsdata::sa42016
} else {
  sa42016 <- sa42016
}


# Load data from djprdashdata if it has been updated
temp_loc <- tempfile(fileext = ".rds")
utils::download.file(
  url = "https://github.com/djpr-data/djprdashdata/blob/main/data-raw/last_updated.rds?raw=true",
  destfile = temp_loc
)

remote_updated <- readRDS(temp_loc)
remote_updated <- as.POSIXct(remote_updated)

dash_data_updated <- dash_data_updated

if (dash_data_updated != remote_updated) {
  dash_data <- load_dash_data()
  stopifnot(inherits(dash_data, "tbl_df"))
  stopifnot(nrow(dash_data) > 800)
  dash_data_updated <- remote_updated
  # source(file.path("data-raw", "send_briefing_email.R"), local = T)
}

usethis::use_data(sa42016,
  dash_data_updated,
  dash_data,
  internal = TRUE, overwrite = TRUE
)
