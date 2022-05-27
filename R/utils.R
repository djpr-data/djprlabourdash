
# Shiny components
date_slider <- function(
  id,
  table_no = "6202012",
  label = "Dates",
  min = NULL,
  max = NULL,
  value = NULL,
  width = "90%",
  timeFormat = "%b %Y",
  dragRange = TRUE,
  ticks = FALSE,
  ...
){

  if(!exists("data_dates")) stop(
    "Cannot find data_dates. Have you run pkgload::load_all() ?"
    )

  if(is.null(min)){min <- data_dates[[table_no]]$min}
  if(is.null(max)){max <- data_dates[[table_no]]$max}
  if(is.null(value)){
    value <- c(data_dates[[table_no]]$min, data_dates[[table_no]]$max)
    }

  shiny::sliderInput(
    shiny::NS(id, "dates"),
    label = "Dates",
    min = min,
    max = max,
    value = value,
    width = width,
    timeFormat = timeFormat,
    dragRange = dragRange,
    ticks = ticks,
    ...
  )
}

state_checkbox <- function(
  id,
  label = "jurisdiction",
  choices = c("Vic", "NSW", "SA", "QLD", "WA", "NT", "ACT", "Tas"),
  selected =  c("Vic", "NSW"),
  inline = TRUE,
  ...
  ){
  shinyWidgets::awesomeCheckboxGroup(
    shiny::NS(id, "states"),
    label = label,
    choices = choices,
    selected =  selected,
    inline = TRUE,
    ...
  )
}

box <- function(..., width = 6, title = NULL, footer = NULL, height = NULL){

  stopifnot(width %in% 1:12)

  shiny::div(
    class = paste0("col-sm-", width),
    shiny::div(
      class = "box",
      style = if(is.null(height)) NULL else paste0("height:", height,";"),
      if(is.null(title)) NULL else shiny::div(class = "box-header", title),
      ...,
      if(is.null(footer)) NULL else shiny::div(class = "box-footer", footer)
    )
  )

}


column_nopad <- function(width = 6, ...){
  shiny::column(width = width, ...) %>%
    shiny::tagAppendAttributes(style = "padding:0px;")
}


focus_box <- function(
  title,
  inputs,
  ...,
  colour = "#00573f",
  text_colour = "#FFFFFF",
  width = 12,
  title_width = 9
  ){

  # Checks
  if(!(title_width %in% 1:11)) stop(
    "Title width must be in 1:11 (bootstrap grid)\nTitle width cannot be 12 as space is needed for inputs"
    )
  if(!(width %in% 1:12)) stop(
    "Focus box width must be in 1:12 (bootstrap grid)"
  )

  # Options
  title_width <- round(title_width)
  inputs_width <- 12L - title_width
  box_style <- paste0("border: 3px solid ", colour, "; background: ", text_colour)
  title_style <- paste0("background: ", colour, "; padding:10px; margin: 0;")

  # Reformat title
  title <- span(title, style = paste0("color: ", text_colour, ";"))

  # Create box
  box(
    width = width,
    style = box_style,

    fluidRow(
      style = title_style,
      column(
        title_width,
        title
      ),
      column(
        inputs_width,
        inputs
      )
    ),

    div(
      style = "padding:15px;",
      ...
    )
  )

}


# no background box
async_no_background <- function(djpr_async_ui){
  djpr_async_ui$children[[1]] <- tagAppendAttributes(
    djpr_async_ui$children[[1]],
    style = "background:transparent;box-shadow:none;"
  )
  djpr_async_ui$children[[1]]$children <- lapply(
    djpr_async_ui$children[[1]]$children,
    tagAppendAttributes,
    style = "background:transparent;"
  )

  djpr_async_ui
}



