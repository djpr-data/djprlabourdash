##########################################
##########Labour Force Reactable##########
##########################################

#Author: Darren Wong
#QA: -


##Load packages

#' @import readabs
#' @import reactable
#' @import dataui
#' @import tidyverse
#' @export



##Create function

lf_update <- function(startdate="2018-01-01",type="Seasonally Adjusted",people="Persons") {


##Load Labour Force Data

lf <- readabs::read_abs("6202.0", tables = 5)


##Create template table

template <- lf %>%
  filter(
    series_type == type,
    str_detect(series, people), !str_detect(series, "full-time")
  ) %>%
  group_by(series) %>%
  count()


##Clean Labour Force Data

labourforceclean <- lf %>%
  filter(
    series_type == "Seasonally Adjusted",  #Choose seasonally adjusted series rather than trend or original
    str_detect(series, "Persons"),  #Choose total people rather than men or women
    !str_detect(series, "full-time")  #Ignore distinction between full time and part time work
  ) %>%
  select(date, series, value, unit) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(value = ifelse(unit == "000", 1000 * value, value)) %>%  #Convert '000 to per person
  mutate(changeinquarter = (value - lag(value))) %>%  #Create Change in Quarter
  mutate(changeinquarterpc = changeinquarter / lag(value) * 100) %>%  #Create percentage change in quarter
  mutate(changeinquarterpc = sprintf("%.1f %%", changeinquarterpc)) %>%  #Style percentage change in quarter
  mutate(changeinquarter = ifelse(unit == "000", sprintf("%1.0f", changeinquarter), sprintf("%.1f %%", changeinquarter))) %>%  #Style change in quarter
  mutate(changeinyear = (value - lag(value, 4))) %>%  #Create change in year
  mutate(changeinyearpc = changeinyear / lag(value, 4) * 100) %>%  #Create percentage change in year
  mutate(changeinyearpc = sprintf("%.1f %%", changeinyearpc)) %>%  #Style perchange change in year
  mutate(changeinyear = ifelse(unit == "000", sprintf("%1.0f", changeinyear), sprintf("%.1f %%", changeinyear))) %>%  #Style change in quarter
  filter(date > startdate) %>%  #Apply startdate
  ungroup()



##Select only the latest changes in quarter and year

changedf <- labourforceclean %>%
  group_by(series) %>%
  slice(which.max(date)) %>%
  select(date, series, changeinquarter, changeinquarterpc, changeinyear, changeinyearpc) %>%
  ungroup()


##Create function to change time series to a list

fun1 <- function(x) {
  labourforceclean %>%
    filter(series == x) %>%
    select(value) %>%
    as.list()
}


##Create a dataframe in the format required for a sparkline with the list function

sparklinelist <- template %>%
  mutate(n = map(series, fun1)) %>%
  left_join(changedf, by = "series") %>%
  select(-date)


##Define colour palette

colpal <- topo.colors(6)


##Create Reactable

rt1 <- reactable(
  sparklinelist,  #Specify dataframe to use
  columns = list(
    changeinquarter = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(background = color, fontWeight = "bold", color = "#ffffff")  #Conditional format background based on value
      }
    ),
    changeinquarterpc = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(background = color, fontWeight = "bold", color = "#ffffff")  #Conditional format background based on value
      }
    ),
    changeinyear = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(background = color, fontWeight = "bold", color = "#ffffff")  #Conditional format background based on value
      }
    ),
    changeinyearpc = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(background = color, fontWeight = "bold", color = "#ffffff")  #Conditional format background based on value
      }
    ),
    n = colDef(
      cell = function(value, index) {
        dui_sparkline(
          data = value[[1]],
          height = 80,
          components = list(
            dui_sparklineseries(
              stroke = colpal[index],
              showArea = TRUE,
              fill = colpal[index] #Create actual sparkline
            ),
            dui_tooltip(components = list(
              dui_sparkverticalrefline(
                strokeDasharray = "4,4",
                stroke = gray.colors(10)[3]  #Create moving tooltip
              ),
              dui_sparkpointseries(
                stroke = colpal[index],
                fill = "#fff",
                renderLabel = htmlwidgets::JS("(d) => d.toFixed(2)") #display tooltip value
              )
            ))
          )
        )
      }
    )
  ),
  highlight = TRUE,
  resizable = TRUE,
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif")
  )
)

rt1

}
