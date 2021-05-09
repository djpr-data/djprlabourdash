library(shiny)
# https://afeld.github.io/bootstrap-toc/#examples

ui <- fluidPage(
  theme = bslib::bs_theme(version = "4"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.css"
    ),
    tags$script(src = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js"
                # src = "toc.js"
                ),
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
      id = "tab1",
      br(),
      br(),
      br(),
      fluidRow(
        column(
          3,
          br(),
          br(),
          tags$nav(
            id = "toc1",
            class="sticky-top"
          ),
          tags$script(
            '$(function() {
  var navSelector = "#toc1";
  var $myNav = $(navSelector);
  Toc.init({
  $nav: $("#toc1"),
  $scope: $(document.getElementById("page-1-content"))
  });
  $("body").scrollspy({
    target: navSelector
  });
});'
          ),
        ),
        column(
          9,
          id = "page-1-content",
          h1("Page 1 title"),
          h2("Some subtitle"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h3("Section 1"),
          h1("Second page 1 title"),
          h4("Subsection A"),
          h4("Subsection B"),
          h3("Section 2")
        )
      )
    ),
    tabPanel(
      title = "Second tab",
      id = "tab2",
      br(),
      br(),
      fluidRow(
        column(
          3,
          tags$nav(
            id = "toc2",
            class = "sticky-top"
          ),
          tags$script(
            '$(function() {
  var navSelector = "#toc2";
  var $toc2 = $(navSelector);
    Toc.init({
  $nav: $("#toc2"),
  $scope: $(document.getElementById("page-2-content"))
  });
  $("body").scrollspy({
    target: navSelector
  });
});'
          )
        ),
        column(
          9,
          id = "page-2-content",
          h1("Unemployment"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h1("Underemploykent"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h1("Foo bar"),
          paste0(rep("Text goes here", 1e3), collapse = "")
        )
      )
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
