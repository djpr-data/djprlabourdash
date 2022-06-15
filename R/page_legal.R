page_legalUI <- function(...) {
  fluidRow(
    # title = "Legal",
    # value = "tab-legal",
    djpr_h2_box("Legal"),
    br(),
    shiny::includeMarkdown(system.file("disclaimer.md", package = "djprlabourdash")),
    br(),
    shiny::includeMarkdown(system.file("copyright.md", package = "djprlabourdash")),
    br(),
  )
}
