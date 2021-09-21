
knit_briefing <- function(path_out = tempfile(fileext = ".docx")) {

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("knit_briefing() requires the rmarkdown package.\n",
         " Install it with: install.packages('rmarkdown')")
  }

  if (!requireNamespace("officedown", quietly = TRUE)) {
    stop("knit_briefing() requires the officedown package.\n",
         " Install it with: install.packages('officedown')")
  }

  rmd_path <- system.file("jobs_briefing.Rmd", package = "djprlabourdash")

  rmarkdown::render(input = rmd_path,
                    output_file = path_out,
                    quiet = FALSE)

  return(path_out)
}
