# dependencies
if(!require(shiny)) install.packages("shiny")
if(!require(randomForest)) install.packages("randomForest")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(devtools)) install.packages("devtools")
if(!require(shinyIncubator)) devtools::install_github("shiny-incubator", "rstudio")
if(!require(xtable)) install.packages("xtable")

library(shiny)
library(shinyIncubator)
library(shinythemes)
library(randomForest)
library(xtable)