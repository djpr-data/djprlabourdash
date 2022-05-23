
# Shiny components
date_slider <- function(
  id,
  label = "Dates",
  min = dates$min,
  max = dates$max,
  value = c(dates$min, dates$max),
  width = "90%",
  timeFormat = "%b %Y",
  dragRange = TRUE,
  ticks = FALSE,
  ...
){
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

