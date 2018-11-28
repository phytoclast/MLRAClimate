#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
#calculate percentiles
library(plyr)


MLRA <- readRDS(file='data/MLRA.RDS')



n <- sort(unique(MLRA$LRU))
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climate Graphs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("mlra",
                  "Select MLRA:",
                  n)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("climplot")
    )
  )
))
