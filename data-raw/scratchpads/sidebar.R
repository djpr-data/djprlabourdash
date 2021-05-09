library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = "4"),
  navbarPage(
    title = "App title",
    tabPanel(
      title = "Tab title",
      br(),
      br(),
      sidebarLayout(
        sidebarPanel("Lorem ipsum",
          br(),
          "Lorem ipsum",
          style = "position: fixed; height: 10vh; overflow-y: auto;"
        ),
        mainPanel(paste(rep(
          "lorem ipsum ",
          1e3
        ),
        collapse = ""
        ))
      )
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
