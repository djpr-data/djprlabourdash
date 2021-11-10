#' Create a Word document containing briefing tables
#' This function knits a briefing document that uses tables generated in this
#' package to create a Word document. It knits the RMarkdown file
#' `jobs_briefing.Rmd` that lives in the `inst` folder of `djprlabourdash`.
#' @param path Path to directory where
#' the knitted Word document should be created.
#' @param quietly Passd to `rmarkdown::render()`'s `quiet` argument. `FALSE` by
#' default.
#' @examples
#' \dontrun{
#' knit_briefing(quietly = TRUE)
#' }
#' @export

knit_briefing <- function(path = tempdir(),
                          quietly = FALSE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop(
      "knit_briefing() requires the rmarkdown package.\n",
      " Install it with: install.packages('rmarkdown')"
    )
  }

  if (!requireNamespace("officedown", quietly = TRUE)) {
    stop(
      "knit_briefing() requires the officedown package.\n",
      " Install it with: install.packages('officedown')"
    )
  }

  filename <- paste0("DJPR_Jobs_Briefing_", Sys.Date(), ".docx")

  out_path <- file.path(path, filename)
  rmd_path <- system.file("jobs_briefing.Rmd", package = "djprlabourdash")

  rmarkdown::render(
    input = rmd_path,
    output_file = out_path,
    quiet = quietly
  )

  return(out_path)
}
