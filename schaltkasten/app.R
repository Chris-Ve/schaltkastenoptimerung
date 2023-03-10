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
      fluidRow(column(6, actionButton("run", "Berechnen"), align="center"),
                  column(5, checkboxInput("filter", "Ergebnis filtern"),
                         align = "center")),
      hr(),
      tableOutput("tbl_raw"),
      tableOutput("solution"),
      verbatimTextOutput("totalcost")

    ),
    mainPanel(
      fluidRow(column(6, DTOutput("tbl"))))
  )

)

server <- function(input, output, session) {

  html_caption_auswahl <- as.character(shiny::tags$b(style = "color: black", "Verwendbare Module:"))
  html_caption_ergebnis <- as.character(shiny::tags$b(style = "color: green", "Berechnete Module:"))

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


  N <- 7
  names <- c(LETTERS[1:N])
  # min_use <- rep(0, 5)
  modules <- matrix(c(0,8,0,0,0,0,
                      0,0,16,0,0,0,
                      12,0,0,0,0,0,
                      8,0,0,0,0,0,
                      0,0,0,0,16,0,
                      0,4,0,0,8,0,
                      8,0,0,0,0,28),
                    7,6,byrow = TRUE)
  costs <- c(623,404,638,598,638,837,1676)


  df <- data.frame(Bezeichnung = names, modules, Preis = costs)
  table <- data.frame(df,
                      Mindestanzahl = shinyInput(numericInput, N, 'min_use_', value=0, min=0, width = "70px"),
                      Verwenden = shinyInput(checkboxInput, N, 'use_', value=TRUE, width = "50px"),
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


  output$tbl_raw = renderTable({data.frame(df, Mindestanzahl = shinyValue("min_use_", N)) |>
      subset(shinyValue("use_", N))
  }, rownames = FALSE, striped = TRUE, digits = 0,
  caption = html_caption_auswahl,
  caption.placement = getOption("xtable.caption.placement", "top"))



  solution <- eventReactive(input$run, {

    costs_ <- costs[shinyValue("use_", N)]

    mods <- modules[shinyValue("use_", N), , drop=FALSE]

    n <- sum(shinyValue("use_", N))

    model <- MIPModel() |>
      add_variable(modul[i], i = 1:n, type = "integer") |>
      set_bounds(modul[i], i = 1:n, lb = 0) |>
      set_objective(sum_over(costs_[i] * modul[i], i = 1:n), "min") |>
      add_constraint(sum_over(mods[i,1] * modul[i], i = 1:n) >= input$ai) |>
      add_constraint(sum_over(mods[i,2] * modul[i], i = 1:n) +
                       sum_over(mods[i,6] * modul[i], i = 1:n) >= input$ao) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(mods[i, 2] * modul[i], i = 1:n) +
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n) +
                       input$do -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(mods[i, 2] * modul[i], i = 1:n) +
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(mods[i, 2] * modul[i], i = 1:n) +
                       input$do -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(mods[i, 2] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n) +
                       input$do -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$do -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       0)

    model$variable_bounds_lower <- shinyValue("min_use_", N)[shinyValue("use_", N)]

    model <- model |>
      solve_model(with_ROI(solver = "glpk")) |>
      get_solution(modul[i])

    data.frame(Modul = names[shinyValue("use_", N)],
               Preis = costs[shinyValue("use_", N)],
               Anzahl = model$value)

  })

  output$solution <- renderTable({subset(solution(), Anzahl > input$filter-1)},
                                 rownames = FALSE, striped = TRUE, digits = 0,
                                 caption = html_caption_ergebnis,
                                 caption.placement = getOption("xtable.caption.placement", "top"))

  # output$solution <- renderPrint({data.frame(Modul = names[shinyValue("use_", 5)],
  #                                            Anzahl = solution()$value)})

  output$totalcost <- renderPrint({cat("Kosten: ", sum(solution()$Preis * solution()$Anzahl))})
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
