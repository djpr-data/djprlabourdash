

#' @title Update Package Data and Knit Report
#' @description This Rstudio addin will run the code to update data,
#' clean the app-cache and then render the Monthly report
#' @export
update_data_and_knit <- function() {
  stopifnot(grepl("djprlabourdash", rstudioapi::getActiveProject()))

  continue <- readline(prompt = "Has the update to djprdashdata completed? (y/n): ")
  assert_that(tolower(continue) == "y", msg = "You need to wait for djprdashdata update!")

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
      unlink(here::here("app-cache/*"))
      fs::dir_delete(here::here("app-cache"))
      cat(crayon::green("    SUCCESS\n\n"))
    },
    error = function(e) {
      cat(crayon::red("FAILED, please delete app-cache folder manually\n\n"))
    }
  )


  cat(crayon::yellow("Render RMD report to Word:\n"))
  tryCatch(
    {
      knit_briefing()
      cat(crayon::green("    SUCCESS\n\n"))
    },
    error = function(e) {
      cat(crayon::red("FAILED, please knit inst/jobs_briefing.Rmd manually\n\n"))
    }
  )
}
