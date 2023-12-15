# export internal KITE functions
`%diag%` = KITE:::`%diag%`
cast_variable = KITE:::cast_variable
initialize_variable = KITE:::initialize_variable
vector_to_array = KITE:::vector_to_array
melt_variable = KITE:::melt_variable

# compute welfare change
compute_welfare_change = function(results){
  
  # reshape results ----
  initial_conditions = lapply(results$initial_conditions, cast_variable)
  model_scenario = lapply(results$model_scenario, cast_variable)
  output = lapply(results$output, cast_variable)
  settings = results$settings
  
  # create tariff arrays ----
  if (is.null(initial_conditions$tariff)) initial_conditions$tariff = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(model_scenario$tariff_new)) model_scenario$tariff_new = initial_conditions$tariff
  model_scenario$tariff_change = model_scenario$tariff_new / initial_conditions$tariff
  
  # create ntb arrays ----
  if (is.null(initial_conditions$ntb)) initial_conditions$ntb = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(model_scenario$ntb_new)) model_scenario$ntb_new = initial_conditions$ntb
  if (is.null(model_scenario$ntb_change)) model_scenario$ntb_change = model_scenario$ntb_new / initial_conditions$ntb
  
  # create export subsidies arrays ----
  if (is.null(initial_conditions$export_subsidy)) initial_conditions$export_subsidy = initialize_variable(settings$model_dimensions[c("origin", "destination", "sector")])
  if (is.null(model_scenario$export_subsidy_new)) model_scenario$export_subsidy_new = initial_conditions$export_subsidy
  model_scenario$export_subsidy_change = model_scenario$export_subsidy_new / initial_conditions$export_subsidy
  
  # create trade balance arrays ----
  if (is.null(initial_conditions$trade_balance)) initial_conditions$trade_balance = initialize_variable(settings$model_dimensions[c("country")])
  if (is.null(model_scenario$trade_balance_new)) model_scenario$trade_balance_new = initial_conditions$trade_balance
  
  # compute price index change ----
  output$price_index_change = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$price_index_change[[c]] =  exp(sum(initial_conditions$consumption_share[,c] * log(output$price_change)[,c]))
  
  # compute tariff revenue ----
  output$tariff_revenue = initialize_variable(settings$model_dimensions[c("sector", "country")])
  for (c in settings$model_dimensions$country)  output$tariff_revenue[,c] =
    colSums(initial_conditions$expenditure[,c] * ((initial_conditions$tariff[,c,] - 1) * initial_conditions$trade_share[,c,] / initial_conditions$tariff[,c,])) # tariff revenue
  
  output$tariff_revenue_new = initialize_variable(settings$model_dimensions[c("sector", "country")])
  for (c in settings$model_dimensions$country)  output$tariff_revenue_new[,c] =
    colSums(output$expenditure_new[,c] * ((model_scenario$tariff_new[,c,] - 1) * output$trade_share_new[,c,] / model_scenario$tariff_new[,c,])) # tariff revenue
  
  output$tariff_revenue_change = output$tariff_revenue_new / output$tariff_revenue
  output$tariff_revenue_change[is.nan(output$tariff_revenue_change)] = 1
  
  # compute export subsidy costs ----
  output$export_subsidy_costs = initialize_variable(settings$model_dimensions[c("sector", "country")])
  for (c in settings$model_dimensions$country) output$export_subsidy_costs[,c] = # Note: is this correct? Answer: I think so, yes.
    initial_conditions$expenditure %diag% ((initial_conditions$export_subsidy[c,,] - 1) * initial_conditions$trade_share[c,,] / (initial_conditions$tariff[c,,] * initial_conditions$export_subsidy[c,,]))
  
  output$export_subsidy_costs_new = initialize_variable(settings$model_dimensions[c("sector", "country")])
  for (c in settings$model_dimensions$country) output$export_subsidy_costs_new[,c] = # Note: is this correct? Answer: I think so, yes.
    output$expenditure_new %diag% ((model_scenario$export_subsidy_new[c,,] - 1) * output$trade_share_new[c,,] / (model_scenario$tariff_new[c,,] * model_scenario$export_subsidy_new[c,,]))
  
  output$export_subsidy_costs_change = output$export_subsidy_costs_new / output$export_subsidy_costs
  output$export_subsidy_costs_change[is.nan(output$export_subsidy_costs_change)] = 1
  
  # compute income ----
  output$income = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$income[[c]] =
    initial_conditions$value_added[c] + sum(output$tariff_revenue[,c]) + sum(output$export_subsidy_costs[,c]) - initial_conditions$trade_balance[c]
  
  output$income_new = initialize_variable(settings$model_dimensions[c("country")])
  for (c in settings$model_dimensions$country) output$income_new[[c]] =
    initial_conditions$value_added[c] * output$wage_change[c] + sum(output$tariff_revenue_new[,c]) + sum(output$export_subsidy_costs_new[,c]) - model_scenario$trade_balance_new[c]
  
  output$income_change = output$income_new / output$income
  
  # compute welfare ----
  welfare_change = (output$income_change / output$price_index_change - 1) * 100

  # melt output ----
  welfare_change = melt_variable(welfare_change)
  
  # return results ----
  return (welfare_change)
  
}
