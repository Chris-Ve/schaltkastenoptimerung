#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

fluidPage(mainPanel(tabsetPanel(
  tabPanel("Plot",
           actionButton("run", "Run"),
           DT::dataTableOutput('x1'),
           verbatimTextOutput('x2'),
           verbatimTextOutput("solution"),
           textOutput("check")),
  tabPanel(
    "Inputs",
    splitLayout(
      numericInput("ai", "AI", value = 0, min = 0),
      numericInput("ao", "AO", value = 0, min = 0),
      numericInput("di", "DI", value = 0, min = 0),
      numericInput("do", "DO", value = 0, min = 0)
    )
  )
)))