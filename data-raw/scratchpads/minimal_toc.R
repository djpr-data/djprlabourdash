library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(version = "4"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.css"
    ),
    tags$script(src = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js"),
    tags$body(
      `data-spy` = "scroll",
      `data-target` = "#toc"
    ),
    tags$style(HTML("
                    .sticky-top {
    top: 100px; /* height of header */
}
                    "))
  ),
  fluidRow(
    column(
      3,
      tags$nav(
        id = "toc",
        `data-toggle` = "toc"
      )
    ),
    column(
      9,
      h1("The title"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h2(br(), "Some sub-title"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h3("Section 1"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h4("Subsection A"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h3("Section 2"),
      paste0(rep("Text goes here", 100), collapse = ""),
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
