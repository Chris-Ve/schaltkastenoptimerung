
library(shiny)
library(DT)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)



fileurl <- "https://raw.githubusercontent.com/Chris-Ve/schaltkastenoptimerung/main/Modulliste.csv"
df <- read.delim(fileurl, header = TRUE, sep = ";")
df$Preis <- gsub("[€.]", "", df$Preis)
df$Preis <- gsub("[,]", ".", df$Preis) |> as.numeric()


ui <- fluidPage(
  navbarPage("Schaltkastenoptimierung"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      splitLayout(
                numericInput("do", "DO", value = 0, min = 0),
                numericInput("ao", "AO", value = 0, min = 0),
                numericInput("di", "DI", value = 0, min = 0),
                numericInput("ai", "AI", value = 0, min = 0)
              ),
      hr(),
      fluidRow(column(6, actionButton("run", "Berechnen"), align="center"),
                  column(5, checkboxInput("filter", "Ergebnis filtern"),
                         align = "center")),
      hr(),
      tableOutput("tbl_raw"),
      tableOutput("solution"),
      tableOutput("total_ios"),
      verbatimTextOutput("totalcost")

    ),
    mainPanel(
      fluidRow(column(6, DTOutput("tbl"))))
  )

)

server <- function(input, output, session) {

  html_caption_auswahl <- as.character(shiny::tags$b(style = "color: black", "Verwendbare Module:"))
  html_caption_ergebnis <- as.character(shiny::tags$b(style = "color: green", "Berechnete Module:"))
  html_caption_total_ios <- as.character(shiny::tags$b(style = "color: green", "Berechnete IOs:"))

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






  N <- nrow(df)
  names <- df$Bezeichnung
  modules <- df[,3:8]
  costs <- df$Preis


  table <- data.frame(df,
                      Mindestanzahl = shinyInput(numericInput, N, 'min_use_', value=0, min=0, width = "70px"),
                      Verwenden = shinyInput(checkboxInput, N, 'use_', value=FALSE, width = "50px"),
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
      add_constraint(sum_over(mods[i,1] * modul[i], i = 1:n) >= input$do) |>
      add_constraint(sum_over(mods[i,2] * modul[i], i = 1:n) +
                       sum_over(mods[i,6] * modul[i], i = 1:n) >= input$ao) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(mods[i, 2] * modul[i], i = 1:n) +
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n) +
                       input$ai -
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
                       input$ai -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ao -
                       sum_over(mods[i, 2] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n) +
                       input$ai -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$di -
                       sum_over(mods[i, 3] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       input$ai -
                       sum_over(mods[i,4] * modul[i], i = 1:n)) |>
      add_constraint(sum_over(mods[i,5] * modul[i], i = 1:n) +
                       sum_over(mods[i, 6] * modul[i], i = 1:n) >=
                       0)

    model$variable_bounds_lower <- shinyValue("min_use_", N)[shinyValue("use_", N)]

    model <- model |>
      solve_model(with_ROI(solver = "glpk")) |>
      get_solution(modul[i])


    data.frame(subset(df, shinyValue("use_", N)),
               Anzahl = model$value)

  })

  output$solution <- renderTable({subset(solution(), Anzahl > input$filter-1)},
                                 rownames = FALSE, striped = TRUE, digits = 0,
                                 caption = html_caption_ergebnis,
                                 caption.placement = getOption("xtable.caption.placement", "top"))


  output$total_ios <- renderTable({solution()$Anzahl%*%as.matrix(solution()[,3:8])},
                                   rownames = FALSE, striped = TRUE, digits = 0,
                                   caption = html_caption_total_ios,
                                   caption.placement = getOption("xtable.caption.placement", "top"))

  output$totalcost <- renderPrint({cat("Kosten: ", sum(solution()$Preis * solution()$Anzahl))})
}








# Run the application
shinyApp(ui = ui, server = server)
