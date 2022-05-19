

#' @title Update Package Data and Knit Report
#' @description This Rstudio addin will run the code to update data, and
#' clean the app-cache
#'
#' @importFrom assertthat assert_that
#' @return
#' @export
#'
#' @examples
update_data <- function() {
  stopifnot(grepl("djprlabourdash", rstudioapi::getActiveProject()))

  continue <- readline(prompt="Has the update to djprdashdata package completed? (y/n): ")
  assertthat::assert_that(tolower(continue) == 'y', msg = 'You need to wait for djprdashdata update!')

  cat(crayon::yellow("Update Labour Force Data:\n"))
  tryCatch(
    {
      source(here::here("data-raw/internal_data.R"))
      cat(crayon::green("    SUCCESS\n\n"))
    },
    error = function(e) {
      cat(crayon::red("FAILED, please run data-raw/internal_data.R manually\n\n"))
    }
  )


  cat(crayon::yellow("Clean Application Cache Data:\n"))
  tryCatch(
    {
      if (fs::dir_exists(here::here("app-cache"))) {
        unlink(here::here("app-cache/*"))
        fs::dir_delete(here::here("app-cache"))
        cat(crayon::green("    SUCCESS\n\n"))
      } else {
        cat(crayon::green("    Already Deleted\n\n"))
      }
    },
    error = function(e) {
      cat(crayon::red("FAILED, please delete app-cache folder manually\n\n"))
    }
  )

  cat(crayon::yellow('** REINSTALL PACKAGE TO ENSURE NEW DATA IS AVAILABLE **'))

}



#' @title Knit Report to Word
#' @description This Rstudio addin will run the code to render the Monthly report.
#'
#' @return
#' @export
#'
#' @examples
generate_report <- function() {

  # ensure data is up to date
  dash_data_date <- readLines('https://raw.githubusercontent.com/djpr-data/djprdashdata/main/data-raw/last_updated.txt')
  assertthat::assert_that(dash_data_date == djprlabourdash::dash_data_updated,
                          msg = 'something went wrong with the data update please redo')

  show_output <- switch(Sys.getenv()[['RSTUDIO_PROGRAM_MODE']],
                     desktop = TRUE,
                     server = FALSE)


  cat(crayon::yellow("Render RMD report to Word:\n"))
  tryCatch(
    {
      knit_briefing(show = show_output)
      cat(crayon::green("    SUCCESS\n\n"))
    },
    error = function(e) {
      cat(crayon::red("FAILED, please knit inst/jobs_briefing.Rmd manually\n\n"))
    }
  )



}
