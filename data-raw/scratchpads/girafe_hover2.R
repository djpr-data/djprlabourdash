
# UI -----
library(ggiraph)

ui <- shinyUI(fluidPage(
  fluidRow(
      checkboxInput(
        "opt_hover_data",
        label = "Reactive data hovering",
        value = T
      ),
      checkboxInput(
        "hover_inv",
        label = "Hovering over one data element, makes the rest semi-transparent",
        value = FALSE
      )
  ),
  fluidRow(column(width = 8,
                  girafeOutput("plot")),
           column(width = 4,
                  uiOutput("checkboxes")))
))

# Module -----
library(shiny)

StateHandlerUI <- function(id, label, choices, selected) {
  ns <- NS(id)

  checkboxGroupInput(inputId = ns("chk"),
                     label = label,
                     selected = selected,
                     choices = choices)
}

StateHandler <- function(input,
                         output,
                         session,
                         plotInput,
                         messageId) {
    plot_data <- reactive({
    v <- do.call(plotInput, list())
    if (is.null(v)) {
      out <- character(0)
    } else {
      out <- v
    }
    out
  })

  chk_data <- debounce(reactive({
    if (is.null(input$chk)) {
      out <- character(0)
    } else {
      out <- input$chk
    }
    print(class(out))
    out
  }), 200)

  ignore_plot_data <- FALSE

  ignore_chk_data <- FALSE

  observeEvent(plot_data(), {
      # ignore_chk_data  <<- TRUE
      updateCheckboxGroupInput(session,
                               'chk',
                               selected = plot_data())
  })

  observeEvent(chk_data(), {
    print(chk_data())
      session$sendCustomMessage(type = "plot_hovered_set", message = chk_data())
  })

  return(list(plot_data, chk_data))
}

# Server ----
library(ggplot2)
library(ggiraph)
library(dplyr)

options(shiny.trace = FALSE)

dat <- data.frame(
  name = c("Christine", "Ginette", "David", "Cedric", "Frederic"),
  gender = c("Female", "Female", "Male", "Male", "Male"),
  height = c(169, 160, 171, 172, 171)
)

p <- ggplot(dat,
            aes(
              x = name,
              y = height,
              data_id = name,
              tooltip = name
            )) +
  geom_bar_interactive(stat = "identity") +
  theme_minimal()


server <- shinyServer(function(input, output, session) {
  output$plot <- renderGirafe({
    x <- girafe(ggobj = p,
                width_svg = 6,
                height_svg = 8)

    x <- girafe_options(
      x,
      opts_hover(reactive = input$opt_hover_data),
      opts_hover_inv(css = if (input$hover_inv == TRUE) "opacity:0.3" else "")
    )
    x
  })

  output$checkboxes <- renderUI({
        StateHandlerUI(
          "hovered_data",
          label = "Hovered data elements:",
          choices = list("Christine", "Ginette", "David", "Cedric", "Frederic"),
          selected = "Ginette"
        )
  })

  # link_selection_default <- FALSE
  link_hover_default <- FALSE

  callModule(StateHandler,
             id = 'hovered_data',
             plotInput = reactive(input$plot_hovered),
             messageId = 'plot_hovered_set')


  observeEvent(input$link_hover, {
    link_hover_default <<- input$link_hover
  })



})

shiny::shinyApp(ui, server)
