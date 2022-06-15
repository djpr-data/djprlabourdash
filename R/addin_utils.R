

addins_check_env <- function(){

  checks <- c('RSTUDIO_PROGRAM_MODE',
              'R_USER_HOME',
              'USEREMAIL',
              'SMS_LABOURFORCE',
              'SMS_LABOURFORCE_TEST') |>
    purrr::map(function(x){
      tryCatch({
        assertthat::assert_that(x %in% names(Sys.getenv()),
                                msg = glue::glue('add {x} to .Renviron'))
        }, error = function(e){
          print(e)
        }
      )
    }) |> unlist()

  # check for distribution list path
  email_lists_url <- file.path(Sys.getenv()[["R_USER_HOME"]],
                               "VicGov",
                               "Economic Modelling Team - Documents",
                               "Modelling research projects",
                               "Labour market dashboard",
                               "Jobs dashboard circulation list.xlsx",
                               fsep = .Platform$file.sep
  )

  check_filepath <- TRUE

  if (Sys.getenv()[['RSTUDIO_PROGRAM_MODE']] == 'desktop') {
    check_filepath <- tryCatch({
      assertthat::assert_that(fs::file_exists(email_lists_url),
                              msg = 'you must sync the "Economic Modelling Team" Document Library in OneDrive')
    }, error = function(e){
      print(e)
    })

  }

  if (any(checks) != 'TRUE' | !isTRUE(check_filepath)) {
    usethis::edit_r_environ()
    FALSE
  } else {TRUE}

}
