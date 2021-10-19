#' Create a Word document containing briefing tables
#' This function knits a briefing document that uses tables generated in this
#' package to create a Word document. It knits the RMarkdown file
#' `jobs_briefing.Rmd` that lives in the `inst` folder of `djprlabourdash`.
#' @param path_out Path, including filename and extension, to the file where
#' the knitted Word document should be created.
#' @param quietly Passd to `rmarkdown::render()`'s `quiet` argument. `FALSE` by
#' default.
#' @examples
#' \dontrun{
#' knit_briefing(quietly = TRUE)
#' }
#' @export

knit_briefing <- function(path_out = file.path(
                            tempdir(),
                            "djpr_jobs_briefing.docx"
                          ),
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

  rmd_path <- system.file("jobs_briefing.Rmd", package = "djprlabourdash")

  rmarkdown::render(
    input = rmd_path,
    output_file = path_out,
    quiet = quietly
  )

  return(path_out)
}
