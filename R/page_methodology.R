page_methodologyUI <- function(...) {
  fluidRow(
    # title = "Notes",
    # value = "tab-notes",
    djpr_h2_box("Notes"),
    br(),
    shiny::includeMarkdown(system.file("methodology.md", package = "djprlabourdash")),
    br(),
    shiny::includeMarkdown(system.file("glossary.md", package = "djprlabourdash")),
    shiny::br(),
    shiny::includeMarkdown(system.file("note.md", package = "djprlabourdash")),
    shiny::br(),
    # centred_row(htmlOutput("methodology_footnote")),
    box(
      width = 12,
      style = "padding:10px;",
      shiny::div(style = 'position:right;bottom:20px',
                 shiny::actionLink('frommethodology_tolegal',
                                   "Disclaimer and Copyright",
                                   style = 'material-flat',
                                   color = 'success')
      )
    )
  )
}
