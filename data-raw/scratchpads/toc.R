library(shiny)
# https://afeld.github.io/bootstrap-toc/#examples

ui <- fluidPage(
  theme = bslib::bs_theme(version = "4"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.css"
    ),
    tags$script(src = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js"),
    tags$style(HTML("
                    .sticky-top {
    top: 100px; /* height of header */
}
                    "))
  ),
  tags$body(
    `data-spy` = "scroll",
    `data-target` = "#toc"
  ),
  navbarPage(
    title = "App title",
    position = "fixed-top",
    tabPanel(
      title = "First tab",
      br(),
      br(),
      br(),
      fluidRow(
        column(
          3,
          br(),
          br(),
          tags$nav(
            id = "toc-1",
            `data-toggle` = "toc",
            class = "sticky-top"
          )
        ),
        column(
          9,
          # h1("The title"),
          h2("Some subtitle"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h3("Section 1"),
          h4("Subsection A"),
          h4("Subsection B"),
          h3("Section 2")
        )
      )
    ),
    tabPanel(
      title = "Second tab",
      br(),
      br(),
      fluidRow(
        column(
          3,
          tags$nav(
            id = "toc-2",
            `data-toggle` = "toc",
            class = "sticky-top"
          )
        ),
        column(
          9,
          # h1("The title"),
          h2("Some other subtitle"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h3("Section 1"),
          h4("Subsection A"),
          h4("Subsection B"),
          h3("Section 2")
        )
      )
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
