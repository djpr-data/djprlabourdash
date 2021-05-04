## code to create `crosstabs` goes here

crosstabs <- djprdashdata::lfs_lookup %>%
  dplyr::select(-dplyr::one_of(c("cat_no",
                                 "table_no",
                                 "series",
                                 "series_type"))
  )

usethis::use_data(crosstabs, internal = TRUE, overwrite = TRUE)
