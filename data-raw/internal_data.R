## Create internal data

# Load absmapsdata object(s) to avoid dependency
sa42016 <- absmapsdata::sa42016


usethis::use_data(sa42016, internal = TRUE, overwrite = TRUE)
