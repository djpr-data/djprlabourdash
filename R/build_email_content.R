


#' Merge Message Contents into Single Document
#'
#' @return
#' @export
#'
#' @examples
build_email_content <- function() {
  stopifnot(grepl("djprlabourdash", rstudioapi::getActiveProject()))

  writeLines(
    c(
      readLines("inst/preamble.md"),
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
