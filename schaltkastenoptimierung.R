install.packages("ompr")
install.packages("ompr.roi")
install.packages("ROI.plugin.glpk")
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

MIPModel() |>
  add_variable(x, "integer") |>
  add_variable(x, "integer")



n <- 7
modules_mat <- matrix(c(8,0,0,0,0,0,
                        0,6,4,0,0,0,
                        0,4,4,4,0,0,
                        2,2,0,0,12,0,
                        0,0,0,0,0,18), 5, 6, byrow = TRUE)
modules_mat <- matrix(c(0,8,0,0,0,0,
                        0,0,16,0,0,0,
                        12,0,0,0,0,0,
                        8,0,0,0,0,0,
                        0,0,0,0,16,0,
                        0,4,0,0,8,0,
                        8,0,0,0,0,28),
                      7,6,byrow = TRUE)
c <- c(10,10,10,10)
costs <- c(623,404,638,598,638,837,1676)
modules <- sample.int(10)
res <- MIPModel() |>
  add_variable(x[i], i = 1:n, type = "integer") |>
  set_bounds(x[i], i = 1:n, lb = 0) |>
  set_objective(sum_over(costs[i] * x[i], i = 1:n), "min") |>
  add_constraint(sum_over(modules_mat[i,1] * x[i], i = 1:n) >= c[1]) |>
  add_constraint(sum_over(modules_mat[i,2] * x[i], i = 1:n) >= c[2]) |>
  add_constraint(sum_over(modules_mat[i,3] * x[i], i = 1:n) >= c[3]) |>
  add_constraint(sum_over(modules_mat[i,4] * x[i], i = 1:n) >= c[4]) |>
  solve_model(with_ROI(solver = "glpk"))
get_solution(res, x[i])

modules_mat
costs
c

res3 <- MIPModel() |>
  add_variable(modul[i], i = 1:n, type = "integer") |>
  set_bounds(modul[i], i = 1:n, lb = 0) |>
  set_bounds(modul[3], lb = 1) |>
  set_objective(sum_over(costs[i] * modul[i], i = 1:n), "min") |>
  add_constraint(sum_over(modules_mat[i,1] * modul[i], i = 1:n) >= c[1]) |>
  add_constraint(sum_over(modules_mat[i,2] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i,6] * modul[i], i = 1:n) >= c[2]) |>
  add_constraint(sum_over(modules_mat[i,3] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,4] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,6] * modul[i], i = 1:n)>= sum(c[3:4])) |>
  add_constraint(sum_over(modules_mat[i,3] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,6] * modul[i], i = 1:n) >= sum(c[3])) |>
  add_constraint(sum_expr(modules_mat[i,4] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                 sum_over(modules_mat[i,6] * modul[i], i = 1:n) >= sum(c[4])) |>
  solve_model(with_ROI(solver = "glpk"))
get_solution(res3, modul[i])


res2 <- MIPModel() |>
  add_variable(x[i], i = 1:n, type = "integer") |>
  set_bounds(x[i], i = 1:n, lb = 0) |>
  set_objective(sum_over(costs[i] * x[i], i = 1:n), "min") |>
  add_constraint(sum_over(modules_mat[i,1] * x[i], i = 1:n) >= c[1]) |>
  add_constraint(sum_over(modules_mat[i,6] * x[i], i = 1:n) >=
                   c[3] - sum_over(modules_mat[i,3] * x[i], i = 1:n) +
                   abs(c[4] - unlist(sum_over(modules_mat[i,4] * x[i], i = 1:n)))) |>
  solve_model(with_ROI(solver = "glpk"))
get_solution(res2, x[i])



res4 <- MIPModel() |>
  add_variable(modul[i], i = 1:n, type = "integer") |>
  set_bounds(modul[i], i = 1:n, lb = 0) |>
  # set_bounds(modul[5], lb = 3) |>
  set_objective(sum_over(costs[i] * modul[i], i = 1:n), "min") |>
  add_constraint(sum_over(modules_mat[i,1] * modul[i], i = 1:n) >= c[1]) |>
  add_constraint(sum_over(modules_mat[i,2] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i,6] * modul[i], i = 1:n) >= c[2]) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[2] -
                   sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
                   c[3] -
                   sum_over(modules_mat[i, 3] * modul[i], i = 1:n) +
                   c[4] -
                   sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[2] -
                   sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
                   c[3] -
                   sum_over(modules_mat[i, 3] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[2] -
                   sum_over(modules_mat[i, 2] * modul[i], i = 1:n) +
                   c[4] -
                   sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[2] -
                   sum_over(modules_mat[i, 2] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[3] -
                   sum_over(modules_mat[i, 3] * modul[i], i = 1:n) +
                   c[4] -
                   sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[3] -
                   sum_over(modules_mat[i, 3] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   c[4] -
                   sum_over(modules_mat[i,4] * modul[i], i = 1:n)) |>
  add_constraint(sum_over(modules_mat[i,5] * modul[i], i = 1:n) +
                   sum_over(modules_mat[i, 6] * modul[i], i = 1:n) >=
                   0) |>
  solve_model(with_ROI(solver = "glpk"))
get_solution(res4, modul[i])


purrr::map(1:6, ~modules_mat[,.x]*c(0,1,0,1,0,0,1)) |> lapply(sum)
sum(costs * c(0,1,0,1,0,0,1))
sum(costs * c(0,1,0,1,1))
sum(costs * c(0,3,0,1,0))

