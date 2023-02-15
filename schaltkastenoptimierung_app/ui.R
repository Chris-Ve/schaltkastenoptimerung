

library(shiny)


fluidPage(
  navbarPage("Schaltkastenoptimerung"),
  sidebarLayout(
            sidebarPanel(
              width = 3,
              numericInput("awdaw", "jaefe", 5),
              verbatimTextOutput('x1'),
              textOutput("check"),
              splitLayout(
                numericInput("ai", "AI", value = 0, min = 0),
                numericInput("ao", "AO", value = 0, min = 0),
                numericInput("di", "DI", value = 0, min = 0),
                numericInput("do", "DO", value = 0, min = 0)
              ),
              actionButton("run", "Run")
            ),
            mainPanel(
              DT::dataTableOutput('x2', width = "70%"),
              verbatimTextOutput("solution"),
              textOutput("check")
            )



))


