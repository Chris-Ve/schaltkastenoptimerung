#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)


ui <- fluidPage(
  navbarPage("Schaltkastenoptimierung"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      splitLayout(
                numericInput("ai", "AI", value = 0, min = 0),
                numericInput("ao", "AO", value = 0, min = 0),
                numericInput("di", "DI", value = 0, min = 0),
                numericInput("do", "DO", value = 0, min = 0)
              ),
      hr(),
      actionButton("run", "Berechnen"),
      hr(),
      verbatimTextOutput("tbl_raw"),
      verbatimTextOutput("vec"),
      verbatimTextOutput("solution")

    ),
    mainPanel(
      fluidRow(column(6, DTOutput("tbl"))))
  )

)

server <- function(input, output, session) {

  shinyInput <- function(FUN, len, id, ...) {
        inputs = character(len)
        for (i in seq_len(len)) {
          inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
        }
        inputs
      }

  shinyValue <- function(id, len) {
        unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
        }))
      }
  n <- 5
  names <- c(LETTERS[1:5])
  # min_use <- rep(0, 5)
  modules <- matrix(c(8, 0, 0, 0, 0, 0,
                      0, 6, 4, 0, 0, 0,
                      0, 4, 4, 4, 0, 0,
                      2, 2, 0, 0, 12, 0,
                      0, 0, 0, 0, 0, 18),
                    nrow = 5, ncol = 6, byrow = TRUE)
  costs <- c(200,180,200,250,320)


  df <- data.frame(names, modules, costs)
  table <- data.frame(df,
                      min_use = shinyInput(numericInput, 5, 'min_use_', value=0, width = "70px"),
                      use = shinyInput(checkboxInput, 5, 'use_', value=TRUE, width = "50px"),
                      stringsAsFactors = FALSE)



  output$tbl <- renderDT(table, server = FALSE,
                         rownames = FALSE,
                         selection = "none",
                         escape = FALSE,
                         editable = list(target = "cell", disable = list(columns = c(0:7))),
                         options = list(pageLength = -1, dom = "ft",
                                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')))



  output$tbl_raw = renderPrint({
    data.frame(df, min_use = shinyValue("min_use_", 5), use = shinyValue("use_", 5 ))
  })

  output$vec <- renderPrint({shinyValue("use_", 5)})



  solution <- eventReactive(input$run, {
    MIPModel() |>
    add_variable(x[i], i = 1:n, type = "integer") |>
    set_bounds(x[i], i = 1:n, lb = 0) |>
    set_objective(sum_over(costs[i] * x[i], i = 1:n), "min") |>
    add_constraint(sum_over(modules[i,1] * x[i], i = 1:n) >= input$ai) |>
    add_constraint(sum_over(modules[i,2] * x[i], i = 1:n) >= input$ao) |>
    add_constraint(sum_over(modules[i,3] * x[i], i = 1:n) >= input$di) |>
    add_constraint(sum_over(modules[i,4] * x[i], i = 1:n) >= input$do) |>
    solve_model(with_ROI(solver = "glpk")) |>
    get_solution(x[i])
  })

  output$solution <- renderPrint({data.frame(solution())})

}












# Define UI for application that draws a histogram
# ui <- fluidPage(
#   navbarPage("Schaltkastenoptimerung"),
#   sidebarLayout(
#     sidebarPanel(
#       width = 3,
#       numericInput("awdaw", "jaefe", 5),
#       verbatimTextOutput('x1'),
#       textOutput("check"),
#       splitLayout(
#         numericInput("ai", "AI", value = 0, min = 0),
#         numericInput("ao", "AO", value = 0, min = 0),
#         numericInput("di", "DI", value = 0, min = 0),
#         numericInput("do", "DO", value = 0, min = 0)
#       ),
#       actionButton("run", "Run")
#     ),
#     mainPanel(
#       fluidRow(column(6,DTOutput("test"))),
#       numericInput("awdaw", "testestestt", 5),
#       DTOutput('x2', width = "70%"),
#       verbatimTextOutput("solution"),
#       textOutput("check")
#     )
#
#
#
#   ))
#
# # Define server logic required to draw a histogram
# server <- function(input, output, session) {
#   modules_mat <- matrix(c(8, 0, 0, 0, 0, 0,
#                       0, 6, 4, 0, 0, 0,
#                       0, 4, 4, 4, 0, 0,
#                       2, 2, 0, 0, 12, 0,
#                       0, 0, 0, 0, 0, 18),
#                     nrow = 5, ncol = 6, byrow = TRUE)
#   costs <- c(200,180,200,250,320)
#
#
#   num <- reactiveVal()
#
#   observeEvent(input$ai, {num(input$ai)} )
#
#   output$check <- renderText({
#     num()
#   })
#
#
#
#
#   name <- c("SXY 100", "SXR", "MPC", "MPC 2", "MPC 3")
#   df <- cbind(name, as.data.frame(modules_mat))
#
#   shinyInput = function(FUN, len, id, ...) {
#     inputs = character(len)
#     for (i in seq_len(len)) {
#       inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
#     }
#     inputs
#   }
#
#   # obtain the values of inputs
#   shinyValue = function(id, len) {
#     unlist(lapply(seq_len(len), function(i) {
#       value = input[[paste0(id, i)]]
#       if (is.null(value)) NA else value
#     }))
#   }
#
#   # a sample data frame
#   res = data.frame(
#     df,
#     use = shinyInput(checkboxInput, 5, 'use_', value = TRUE),
#     stringsAsFactors = FALSE
#   )
#
#   # render the table containing shiny inputs
#   output$x2 = DT::renderDataTable(
#     res, server = FALSE, escape = FALSE, selection = 'none', options = list(
#       preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
#       drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
#     )
#   )
#   # print the values of inputs
#   output$x1 = renderPrint({
#     data.frame(df, use = shinyValue('use_', 5))
#   })
#
#
#
#
#   output$test <- renderDT(iris)
#
#
#
#
#   solution <- eventReactive(input$run, {
#     MIPModel() |>
#       add_variable(modul[i], i = 1:n, type = "integer") |>
#       set_bounds(modul[i], i = 1:n, lb = 0) |>
#       set_objective(sum_over(costs[i] * modul[i], i = 1:n), "min") |>
#       add_constraint(sum_over(modules_mat[i,1] * modul[i], i = 1:n) >= input$ai) |>
#       add_constraint(sum_over(modules_mat[i,2] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i,6] * modul[i], i = 1:n) >= input$ao) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$ao -
#                        sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
#                        input$di -
#                        sum_over(modules_mat[i, 3] * modul[i], i = 1:n) +
#                        input$do -
#                        sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$ao -
#                        sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
#                        input$di -
#                        sum_over(modules_mat[i, 3] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$ao -
#                        sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
#                        input$do -
#                        sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$ao -
#                        sum_over(modules_mat[i, 2] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$di -
#                        sum_over(modules_mat[i, 3] * modul[i], i = 1:n) +
#                        input$do -
#                        sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$di -
#                        sum_over(modules_mat[i, 3] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        input$do -
#                        sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
#       add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
#                        sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
#                        0) |>
#       solve_model(with_ROI(solver = "glpk")) |>
#       get_solution(modul[i])
#   })
#
#
#   output$solution <- renderPrint({data.frame(solution())})
#
# }

# Run the application
shinyApp(ui = ui, server = server)
