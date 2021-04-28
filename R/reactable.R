rm(list = ls())

library(readabs)
library(reactable)
library(dataui)
library(tidyverse)

lf <- readabs::read_abs("6202.0", tables = 5)


#list of numbers

startdate <- as.Date("2016-01-01") #Choose start date


template <- lf %>%
  filter(series_type=="Seasonally Adjusted",
         str_detect(series,"Persons"),!str_detect(series,"full-time")) %>%
  group_by(series) %>%
  count()


labourforceclean <- lf %>%
  filter(series_type=="Seasonally Adjusted",
         str_detect(series,"Persons"),
         !str_detect(series,"full-time")) %>%
  select(date,series,value) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(changeinquarter=(value-lag(value))/lag(value)*100) %>%
  mutate(changeinquarter=sprintf("%.1f %%",changeinquarter)) %>%
  mutate(changeinyear=(value-lag(value,4))/lag(value,4)*100) %>%
  mutate(changeinyear=sprintf("%.1f %%",changeinyear)) %>%
  filter(date>startdate) %>%
  ungroup()

changedf <- labourforceclean %>%
   group_by(series) %>%
  slice(which.max(date)) %>%
  select(date,series,changeinquarter,changeinyear) %>%
  ungroup()


fun1 <- function(x){
  labourforceclean %>%
    filter(series==x) %>%
    select(value) %>%
    as.list()

}




sparklinelist <- template %>%
  mutate(n=map(series,fun1)) %>%
  left_join(changedf,by="series") %>%
  select(-date)

colpal <- topo.colors(6)


rt1 <- reactable(
  sparklinelist,
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
        list(color = color, fontWeight = "bold")
      }),
    changeinyear = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(color = color, fontWeight = "bold")
      }),
    n = colDef(
          cell = function(value, index) {
        dui_sparkline(
          data = value[[1]],
          height = 80,
          components = list(dui_sparklineseries(
            stroke = colpal[index],
            showArea = TRUE,
            fill = colpal[index]),
          dui_tooltip(components = list(
            dui_sparkverticalrefline(strokeDasharray = "4,4",
                                     stroke = gray.colors(10)[3]),
            dui_sparkpointseries(
              stroke = colpal[index],
              fill = "#fff",
              renderLabel = htmlwidgets::JS("(d) => d.toFixed(2)"))
          ))
        ))
      }
    )
    ),
  highlight=TRUE,
  searchable=TRUE,
  )

rt1


changeinquarter = colDef(
  style = function(value) {
    if (value > 0) {
      color <- "#008000"
    } else if (value < 0) {
      color <- "#e00000"
    } else {
      color <- "#777"
    }
    list(color = color, fontWeight = "bold")
  })




