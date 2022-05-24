
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

