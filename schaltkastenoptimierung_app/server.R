

library(shiny)
library(DT)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Define server logic required to draw a histogram
function(input, output, session) {
  modules <- matrix(c(8, 0, 0, 0, 0, 0,
                      0, 6, 4, 0, 0, 0,
                      0, 4, 4, 4, 0, 0,
                      2, 2, 0, 0, 12, 0,
                      0, 0, 0, 0, 0, 18),
                    nrow = 5, ncol = 6, byrow = TRUE)
  costs <- c(200,180,200,250,320)


  num <- reactiveVal()

  observeEvent(input$ai, {num(input$ai)} )

  output$check <- renderText({
    num()
  })




  name <- c("SXY 100", "SXR", "MPC", "MPC 2", "MPC 3")
  df <- cbind(name, as.data.frame(modules_mat))

  shinyInput = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  # obtain the values of inputs
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }

  # a sample data frame
  res = data.frame(
    df,
    use = shinyInput(checkboxInput, 5, 'use_', value = TRUE),
    stringsAsFactors = FALSE
  )

  # render the table containing shiny inputs
  output$x2 = DT::renderDataTable(
    res, server = FALSE, escape = FALSE, selection = 'none', options = list(
      preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )
  )
  # print the values of inputs
  output$x1 = renderPrint({
    data.frame(df, use = shinyValue('use_', 5))
  })




  solution <- eventReactive(input$run, {
    MIPModel() |>
      add_variable(modul[i], i = 1:n, type = "integer") |>
      set_bounds(modul[i], i = 1:n, lb = 0) |>
      set_objective(sum_over(costs[i] * modul[i], i = 1:n), "min") |>
      add_constraint(sum_over(modules_mat[i,1] * modul[i], i = 1:n) >= input$ai) |>
      add_constraint(sum_over(modules_mat[i,2] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i,6] * modul[i], i = 1:n) >= input$ao) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
                       input$di -
                       sum_over(modules_mat[i, 3] * modul[i], i = 1:n) +
                       input$do -
                       sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
                       input$di -
                       sum_over(modules_mat[i, 3] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
                       input$do -
                       sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(modules_mat[i, 2] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$di -
                       sum_over(modules_mat[i, 3] * modul[i], i = 1:n) +
                       input$do -
                       sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$di -
                       sum_over(modules_mat[i, 3] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       input$do -
                       sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                       sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                       0) |>
      solve_model(with_ROI(solver = "glpk")) |>
      get_solution(modul[i])
  })


  output$solution <- renderPrint({data.frame(solution())})

}
