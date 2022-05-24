page_methodologyUI <- function(...) {
  fluidRow(
    # title = "Notes",
    # value = "tab-notes",
    djpr_h2_box("Notes")
    br(),
    shiny::includeMarkdown("R/methodology.md"),
    br(),

    shiny::includeMarkdown("R/glossary.md"),

    shiny::br(),

    shiny::includeMarkdown("R/note.md"),

    shiny::br(),
    centred_row(htmlOutput("methodology_footnote")),
    br()
  )
}

