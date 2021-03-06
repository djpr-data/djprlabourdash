
# Shiny components
date_slider <- function(id,
                        table_no = "6202012",
                        label = "Select dates",
                        min = NULL,
                        max = NULL,
                        value = NULL,
                        width = "50%",
                        timeFormat = "%b %Y",
                        dragRange = TRUE,
                        ticks = FALSE,
                        ...) {
  if (!exists("data_dates")) {
    stop(
      "Cannot find data_dates. Have you run pkgload::load_all() ?"
    )
  }

  if (is.null(min)) {
    min <- data_dates[[table_no]]$min
  }
  if (is.null(max)) {
    max <- data_dates[[table_no]]$max
  }
  if (is.null(value)) {
    value <- c(min, max)
  }

  shiny::sliderInput(
    shiny::NS(id, "dates"),
    label = "Select dates",
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
  label = "Jurisdiction",
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

box <- function(..., width = 6, title = NULL, footer = NULL, height = NULL) {
  stopifnot(width %in% 1:12)

  column(
    width,
    # class = paste0("col-sm-", width),
    shiny::div(
      class = "box",
      style = if (is.null(height)) NULL else paste0("height:", height, ";"),
      if (is.null(title)) NULL else shiny::div(class = "box-header", title),
      ...,
      if (is.null(footer)) NULL else shiny::div(class = "box-footer", footer)
    )
  )
}


# djpr_box_ui shim
djpr_box_ui <- function(...){
  ui      <- djprshiny::djpr_box_ui(...)
  colsize <- ui %>%
    shiny::tagGetAttribute("class") %>%
    substr(., 8, nchar(.))

  ui$attribs$class <- paste0("col-xl-", colsize)
  return(ui)
}





# Column shim
column <- function(width, ...){
  colClass <- paste0("col-xl-", width)
  shiny::div(class = colClass, ...)
}


column_nopad <- function(width = 6, ...) {
  column(width = width, ...) %>%
    shiny::tagAppendAttributes(style = "padding:0px;")
}


focus_box <- function(title,
                      inputs,
                      ...,
                      colour = djprtheme::djpr_royal_blue, # "#2A6FA2"
                      text_colour = "#FFFFFF",
                      width = 12,
                      title_width = 9) {

  # Checks
  if (!(title_width %in% 1:11)) {
    stop(
      "Title width must be in 1:11 (bootstrap grid)\nTitle width cannot be 12 as space is needed for inputs"
    )
  }
  if (!(width %in% 1:12)) {
    stop(
      "Focus box width must be in 1:12 (bootstrap grid)"
    )
  }

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
async_no_background <- function(djpr_box_ui) {
  djpr_box_ui$children[[1]] <- tagAppendAttributes(
    djpr_box_ui$children[[1]],
    style = "background:transparent;box-shadow:none;"
  )
  djpr_box_ui$children[[1]]$children <- lapply(
    djpr_box_ui$children[[1]]$children,
    tagAppendAttributes,
    style = "background:transparent;"
  )

  djpr_box_ui
}




######## DRAFT FUNCTIONS ##############
#######################################



djpr_girafe_box <- function(id, ggobj, widget, ...) {
  shiny::div(
    class = "box",
    shiny::div(
      class = "box-header",
      h3(djprtheme::extract_labs(ggobj, "title")),
      h4(djprtheme::extract_labs(ggobj, "subtitle"))
    ),
    shiny::div(
      id = shiny::NS(id, "container"),
      class = "box-body",
      shiny::div(
        id = shiny::NS(id, "ruler-ppi"),
        style = "width:0.75in;visible:hidden;padding:0px"
      ),
      djpr_with_spinner(
        ggiraph::renderggiraph(ggiraph::ggiraph(ggobj = ggobj), height = height),
        proxy.height = height
      )
    ),
    shiny::div(
      class = "box-footer",
      djprtheme::extract_labs(ggobj, "caption") %>%
        shiny::tagAppendAttributes(class = "djpr-caption"),
      ...
    ),
    djpr_ruler_container()
  )
}

djpr_ruler_container <- function(id) {

  # divs IDs and input names used for ggiraph resizing
  ruler_container <- shiny::NS(id, "container")
  ruler_ppi <- shiny::NS(id, "ruler-ppi")
  ruler_input <- shiny::NS(id, "sizing")

  # JS code to create plot-specific resizing
  shiny::tags$script(
    glue::glue(
      '$(document).on("shiny:connected", function(e) {
  var w = document.getElementById("{ruler_container}").offsetWidth;
  var h = document.getElementById("{ruler_container}").offsetHeight;
  var d =  document.getElementById("{ruler_ppi}").offsetWidth;
  var obj = {width: w, height: h, dpi: d};
  Shiny.setInputValue("{ruler_input}", obj, {priority: "event"});
});
$(window).resize(function(e) {
  var w = document.getElementById("{ruler_container}").offsetWidth;
  var h = document.getElementById("{ruler_container}").offsetHeight;
  var d =  document.getElementById("{ruler_ppi}").offsetWidth;
  var obj = {width: w, height: h, dpi: d};
  Shiny.setInputValue("{ruler_input}", obj, {priority: "event"});
});
$(document).on("shiny:inputchanged", function(e) {
  if(e.name === "tabs"){
    var w = document.getElementById("{ruler_container}").offsetWidth;
    var h = document.getElementById("{ruler_container}").offsetHeight;
    var d =  document.getElementById("{ruler_ppi}").offsetWidth;
    var obj = {width: w, height: h, dpi: d};
    Shiny.setInputValue("{ruler_input}", obj, {priority: "event"});
  }
});'
    )
  )
}

height_sync <- function(
    id1,
    id2,
    sync_elements = c("title", "subtitle", "plot", "caption")
    ){

  elements1 <- paste0(id1, "-", sync_elements)
  elements2 <- paste0(id2, "-", sync_elements)

  sync_jquery <- function(element1, element2){
    glue::glue(
      .comment = "^",
      .open = "[",
      .close = "]",
"
$(document).on('shiny:idle', function() {
$('#[element1]').css('height', '');
$('#[element2]').css('height', '');
var heightOne = $('#[element1]').height();
var heightTwo = $('#[element2]').height();
var heightMax = Math.max(heightOne, heightTwo);
if(heightOne !== heightTwo){
$('#[element1]').height(heightMax);
$('#[element2]').height(heightMax);
}
});
$(window).resize(function(e) {
$('#[element1]').css('height', '');
$('#[element2]').css('height', '');
});
"
    )
  }

  script <- mapply(sync_jquery, elements1, elements2, SIMPLIFY = FALSE)
  script <- paste(script, collapse = "")

  shiny::tags$script(script)

}

