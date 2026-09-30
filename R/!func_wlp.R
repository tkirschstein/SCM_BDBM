library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)


solve_warehouse_location <- function(
    demand = c(25000,15000,18000,12000,20000,22000,21000,19000,17000,16000,15000), 
    fixed_costs = c(450,480,520,470,510,460)*1000, 
    transport_cost_rate = 1.3, 
    distances = rbind(
      c(211,121,281,70,201,171,312,382,383,362,252),
      c(415,325,367,239,353,44,221,226,227,168,72),
      c(341,251,234,117,231,98,264,273,274,254,144),
      c(231,140,255,45,175,178,332,389,390,369,259),
      c(92,6,244,136,121,289,383,499,501,480,370),
      c(91,18,263,160,140,313,360,524,525,504,394))
    ) {
  # demand: data.frame with columns: werk_id, demand_mwh
  # fixed_costs: data.frame with columns: lager_id, cost
  # transport_costs: data.frame with columns: lager_id, werk_id, cost
  
  # check for NAs in distance matrix
  if (any(is.na(distances))) {
    stop("Distance matrix contains NA values.")
  }
  
  n_warehouses <- max(ncol(fixed_costs),length(fixed_costs), na.rm = TRUE)
  n_customers <- max(ncol(demand),length(demand), na.rm = TRUE)
  
  transport_costs <- distances * transport_cost_rate
  
  # Modell aufbauen
  model <- MIPModel() %>%
    # Entscheidungsvariablen
    add_variable(x[i, j], i = 1:n_warehouses, j = 1:n_customers, type = "continuous", lb = 0) %>%
    add_variable(y[i], i = 1:n_warehouses, type = "binary") %>%
    
    # Zielfunktion: Minimierung der Gesamttransport- + Fixkosten
    set_objective(
      sum_expr( x[i, j] * demand[j] * transport_costs[i,j], i = 1:n_warehouses, j = 1:n_customers) +
        sum_expr(y[i] * fixed_costs[i], i = 1:n_warehouses),
      "min"
    ) %>%
    
    # Jede Nachfrage muss genau einmal erfüllt werden
    add_constraint(sum_expr(x[i, j], i = 1:n_warehouses) == 1, j = 1:n_customers) %>%
    
    # Werk kann nur von geöffnetem Lager beliefert werden
    add_constraint(x[i, j] <= y[i], i = 1:n_warehouses, j = 1:n_customers)
  
  # Lösung berechnen
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  
  result
  }
