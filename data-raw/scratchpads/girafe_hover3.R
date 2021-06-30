# devtools::load_all()
library(shiny)
library(ggiraph)
library(tidyverse)

# dash_data <- load_dash_data()

data <- filter_dash_data("A84423354L")

df <- data %>%
    dplyr::slice_tail(n = 6)

p <- df %>%
  ggplot(aes(x = as.character(date), y = value, data_id = as.character(date))) +
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
    last = as.character(max(df$date)),
    ever_hover = FALSE
      )

  observeEvent({isFALSE(hovered$ever_hover)}, {
    print((hovered$last))
    session$sendCustomMessage(type = "plot_hovered_set", message = max(df$date))#hovered$last)
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
