page_methodologyUI <- function(...) {
  djpr_tab_panel(
    # title = "Notes",
    # value = "tab-notes",
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

