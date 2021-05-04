rm(list = ls())

library(readabs)
library(reactable)
library(dataui)
library(tidyverse)

lf <- readabs::read_abs("6202.0", tables = 5)


#list of numbers

startdate <- as.Date("2018-01-01") #Choose start date


template <- lf %>%
  filter(series_type=="Seasonally Adjusted",
         str_detect(series,"Persons"),!str_detect(series,"full-time")) %>%
  group_by(series) %>%
  count()


labourforceclean <- lf %>%
  filter(series_type=="Seasonally Adjusted",
         str_detect(series,"Persons"),
         !str_detect(series,"full-time")) %>%
  select(date,series,value,unit) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(value = ifelse(unit=="000", 1000*value , value)) %>%
  mutate(changeinquarter=(value-lag(value))) %>%
  mutate(changeinquarterpc=changeinquarter/lag(value)*100) %>%
  mutate(changeinquarterpc=sprintf("%.1f %%",changeinquarterpc)) %>%
  mutate(changeinquarter= ifelse(unit=="000",sprintf("%1.0f",changeinquarter),sprintf("%.1f %%",changeinquarter))) %>%
  mutate(changeinyear=(value-lag(value,4))) %>%
  mutate(changeinyearpc=changeinyear/lag(value,4)*100) %>%
  mutate(changeinyearpc=sprintf("%.1f %%",changeinyearpc)) %>%
  mutate(changeinyear= ifelse(unit=="000",sprintf("%1.0f",changeinyear),sprintf("%.1f %%",changeinyear))) %>%
  filter(date>startdate) %>%
  ungroup()

changedf <- labourforceclean %>%
  group_by(series) %>%
  slice(which.max(date)) %>%
  select(date,series,changeinquarter,changeinquarterpc,changeinyear,changeinyearpc) %>%
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
        list(background = color, fontWeight="bold", color="#ffffff")
      }),
    changeinquarterpc = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(background = color, fontWeight="bold", color="#ffffff")
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
        list(background = color, fontWeight="bold", color="#ffffff")
      }),
    changeinyearpc = colDef(
      style = function(value) {
        if (value > 0) {
          color <- "#008000"
        } else if (value < 0) {
          color <- "#e00000"
        } else {
          color <- "#777"
        }
        list(background = color, fontWeight="bold", color="#ffffff")
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
  resizable=TRUE,
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"))
)

rt1

