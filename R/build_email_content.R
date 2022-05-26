


#' Merge Message Contents into Single Document
#'
#' @return
#' @export
#'
#' @examples
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
