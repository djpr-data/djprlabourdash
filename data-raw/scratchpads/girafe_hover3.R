# devtools::load_all()
library(shiny)
library(ggiraph)
library(tidyverse)

# dash_data <- load_dash_data()

data <- filter_dash_data("A84423354L")

df <- data %>%
    dplyr::slice_tail(n = 6)

p <- df %>%
  ggplot(aes(x = date, y = value, data_id = date)) +
  ggiraph::geom_col_interactive(fill = "#BCD3EF") +
  theme_void()

ui <- fluidPage(
  ggiraph::girafeOutput("plot"),
  textOutput("hover_ur"),
  textOutput("text")
)

server <- function(input, output, session) {

  output$plot <- ggiraph::renderGirafe({

    ggiraph::girafe(
      ggobj = p,
      options = list(
        opts_hover(reactive = TRUE),
        opts_selection(type = "none")
      )
    )
  })

  output$text <- renderText({
    input$plot_hovered
  })

  hovered <- reactiveValues(
    last = max(df$date),
    ever_hover = FALSE
      )

  observeEvent({isFALSE(hovered$ever_hover)}, {
    print("foobar")
    session$sendCustomMessage(type = "plot_hovered_set", message = hovered$last)
  })

  observeEvent(input$plot_hovered, {
    hovered$last <<- input$plot_hovered
    hovered$ever_hover <<- TRUE
  })

  output$hover_ur <- renderText({
    df$value[df$date == hovered$last]
  })
}

shinyApp(ui, server)
