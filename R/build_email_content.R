


#' Merge Message Contents into Single Document
#'
#' @export
build_email_content <- function() {
  stopifnot(grepl("djprlabourdash", rstudioapi::getActiveProject()))
  stopifnot(addins_check_env())

  release_type <- readline(prompt="Labour Force `Detailed` release? (y/n): ") |>
    tolower() |>
    match.arg(choices = c('yes','no'))

  detailed <- switch(release_type,
                     yes = TRUE,
                     no = FALSE)

  # get dates for latest data
  ref_dates <- reference_dates(detailed = detailed) # generates preamble
  ref_start <- ref_dates$dates$`Start of Reference Week`
  ref_end <- ref_dates$dates$`End of Reference Week`

  cat(crayon::yellow(
    glue::glue('For reference period of {ref_dates$reference_period}, preample reads:\n\n')
  ))

  cat(ref_dates$preamble)

  writeLines(
    c(
      ref_dates$preamble,
      "",
      readLines("inst/dotpoints.md"),
      "",
      c(
        "Details are available in the [DJPR Jobs Dashboard](https://djpr-spp.shinyapps.io/djprlabourdash/) and in the attached briefing tables.",
        "",
        "If you have any questions, please don't hesitate to get in touch.",
        "",
        "",
        "Regards"
      )
    ),
    "inst/message.md"
  )

  rstudioapi::navigateToFile("inst/message.md")
}





#' Edit Distribution Lists
#'
#' @return
#'
#' @examples
#' \dontrun{
#' edit_email_lists()
#' }
edit_email_lists <- function(){

  if (Sys.getenv()[['RSTUDIO_PROGRAM_MODE']] == 'desktop'){
    email_lists_url <- file.path(Sys.getenv()[["R_USER_HOME"]],
                                 "VicGov",
                                 "Economic Modelling Team - Documents",
                                 "Modelling research projects",
                                 "Labour market dashboard",
                                 "Jobs dashboard circulation list.xlsx",
                                 fsep = .Platform$file.sep
    )
    shell.exec(email_lists_url)
  } else {

    url <- 'https://vicgov.sharepoint.com/:x:/r/sites/VG001671/Documents/Modelling%20research%20projects/Labour%20market%20dashboard/Jobs%20dashboard%20circulation%20list.xlsx?d=w157b0eb871c64a77abd2c086c5103221&csf=1&web=1&e=HaULVh'
    browseURL(url)

  }



}
