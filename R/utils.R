
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
