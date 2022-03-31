# Monthly update procedure

library(tidyverse)
library(lubridate)
library(readabs)
library(glue)
library(rvest)
library(httr)
library(emayili)
library(shiny)
library(miniUI)








# 9 re-establish cache
app()




# 10 Commit & push
shell("git add .")
shell("git git commit -m \"Manual update\"")
# shell("git push")






