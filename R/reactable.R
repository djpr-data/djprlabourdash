rm(list = ls())

library(readabs)
library(reactable)
library(dataui)
library(tidyverse)

lf <- readabs::read_abs("6202.0", tables = 5)


#list of numbers



template <- lf %>%
  filter(series_type=="Seasonally Adjusted",
         str_detect(series,"Persons"),!str_detect(series,"full-time")) %>%
  group_by(series) %>%
  count()


labourforceclean <- lf %>%
  filter(series_type=="Seasonally Adjusted",
         str_detect(series,"Persons"),!str_detect(series,"full-time")) %>%
  select(date,series,value)

fun1 <- function(x){
  labourforceclean %>%
    filter(series==x) %>%
    select(value) %>%
    as.list()

}

sparklinelist <- template %>%
  mutate(n=map(series,fun1))


colpal <- topo.colors(6)


rt1 <- reactable(
  sparklinelist,
  columns = list(
    n = colDef(
          cell = function(value, index) {
        dui_sparkline(
          data = value[[1]],
          height = 80,
          components = list(dui_sparklineseries(
            stroke = colpal[index],
            showArea = TRUE,
            fill = colpal[index]),
          # interactivity added here and unstyled for now
          dui_tooltip(components = list(
            dui_sparkverticalrefline(strokeDasharray = "4,4",
                                     stroke = gray.colors(10)[3]),
            dui_sparkpointseries(      #styling
              stroke = colpal[index],
              fill = "#fff",
              #litle extra interactivity for demostration purposes
              renderLabel = htmlwidgets::JS("(d) => d.toFixed(2)")) #obnoxiously big so it is apparent
          ))
        ))
      }
    )))

rt1







