library(tidyverse)
library(gridExtra)
library(readxl)
library(forecast)

simulation_ids = c(intersect(freq_data$worker_id, severity_data$worker_id), 
                   setdiff(freq_data$worker_id, severity_data$worker_id)[1:round(0.4 * length(setdiff(freq_data$worker_id, severity_data$worker_id)))])

simulation_portfolio = freq_data[freq_data$worker_id %in% simulation_ids, ]

frequency_model = zinb_model
severity_model = gamma_model

freq_pred = predict(frequency_model, newdata = simulation_portfolio, type = 'response')
sev_pred = predict(severity_model, newdata = simulation_portfolio, type = 'response')


number_of_policy_holders = nrow(simulation_portfolio)
theta = frequency_model$theta
expected_claims = freq_pred * simulation_portfolio$exposure
shape = 1 / summary(severity_model)$dispersion
scale = sev_pred / shape
total_over_simulations = numeric(number_of_policy_holders)


#Interest, Inflation and Discount Rates
interest_data = read_excel('../Data/srcsc-2026-interest-and-inflation.xlsx')
interest_data = as.data.frame(interest_data)

short_term_rates = interest_data$yr_1_annual_risk_free
rates_ts = ts(short_term_rates, frequency = 1)
interest_ar1 = arima(rates_ts, order = c(1,0,0))  

simulate_interest = function(n_sim = 1) {
  interest_simulation_matrix = matrix(NA, ncol = 10, nrow = n_sim)
  for(i in 1:n_sim){
    interest_simulation_matrix[i,] = simulate(interest_ar1,  10)
  }
  interest_simulation = data.frame(interest_simulation_matrix)
  colnames(interest_simulation) = c('Year_1', 'Year_2', 'Year_3', 'Year_4', 'Year_5', 'Year_6', 'Year_7', 'Year_8', 'Year_9', 'Year_10')
  return(interest_simulation)
}


inflation = interest_data$Inflation
inflation_ts = ts(inflation, frequency = 1)
inflation_ar1 = arima(inflation_ts, order = c(1,0,0))  

simulate_inflation = function(n_sim = 1) {
  inflation_simulation_matrix = matrix(NA, ncol = 10, nrow = n_sim)
  for(i in 1:n_sim){
    inflation_simulation_matrix[i,] = simulate(inflation_ar1,  10)
  }
  inflation_simulation = data.frame(inflation_simulation_matrix)
  colnames(inflation_simulation) = c('Year_1', 'Year_2', 'Year_3', 'Year_4', 'Year_5', 'Year_6', 'Year_7', 'Year_8', 'Year_9', 'Year_10')
  return(inflation_simulation)
}


simulate_discount = function() {
  inflation_sim = as.numeric(simulate_inflation())
  interest_sim = as.numeric(simulate_interest())
  real_interest = (1 + interest_sim) / (1 + inflation_sim) - 1
  return(cumprod(1 / (1 + real_interest)))
}


#Premiums
simulate_premium = function(nsim = 5000, risk_loading = 0.1) {
  total_over_simulations = numeric(number_of_policy_holders)  
  for (sim in 1:nsim) {   
    claims_per_policy = rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    cost_per_policy = numeric(number_of_policy_holders)
    for (i in seq_along(claims_per_policy)) {
      cost_per_policy[i] = sum(
        rgamma(claims_per_policy[i], shape = shape, scale = scale[i])
      )
    }
    total_over_simulations = total_over_simulations + cost_per_policy
  }
  out = data.frame(worker_id = simulation_portfolio$worker_id, expected_premium = total_over_simulations / nsim)
  out$expected_premium = out$expected_premium * (1 + risk_loading)
  return(out)
}

simulate_discounted_premium = function(base_premium = annual_premium_revenue, nsim = 5000) {
  premium_matrix = matrix(NA, nrow = nsim, ncol = 10)
  for (sim in 1:nsim) {
    inflation_sim = as.numeric(simulate_inflation())
    interest_sim = as.numeric(simulate_interest())
    real_interest = (1 + interest_sim) / (1 + inflation_sim) - 1
    discount_factors = cumprod(1 / (1 + real_interest))
    premium_stream = rep(base_premium, 10)
    premium_matrix[sim, ] = premium_stream * discount_factors
  }
  colnames(premium_matrix) = paste0("Year_", 1:10)
  return(premium_matrix)
}


premium_per_worker = simulate_premium() 
annual_premium_revenue = sum(premium_per_worker$expected_premium)

long_term_revenue_per_year = simulate_discounted_premium()
long_term_revenue_per_year = data.frame(long_term_revenue_per_year)

#Short Term Performance
simulate_cost = function(nsim = 5000) { 
  number_of_policy_holders = length(expected_claims)
  cost_per_sim = numeric(nsim)
  for (i in 1:nsim) {
    sim_cost = 0
    number_of_claims = rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    for (j in seq_along(number_of_claims)) {
      if (number_of_claims[j] > 0) {
        sim_cost = sim_cost + sum(rgamma(number_of_claims[j], shape = shape, scale = scale[j]))
      }
    }
    cost_per_sim[i] = sim_cost
  }
  return(data.frame(sim = 1:nsim, total_cost = cost_per_sim))
}
  
short_term_cost = simulate_cost()
short_term_profit = data.frame(sim = 1:5000, profit = annual_premium_revenue - short_term_cost$total_cost)

short_term_losses = short_term_profit %>%
  filter(profit < 0)

colnames(short_term_losses) = c('sim', 'loss')
short_term_losses$loss = abs(short_term_losses$loss)

short_term_loss_summary = data.frame(mean = mean(short_term_losses$loss),
                                     standard_dev = sd(short_term_losses$loss),
                                     var95 = as.numeric(quantile(short_term_losses$loss, 0.95)),
                                     var99 = as.numeric(quantile(short_term_losses$loss, 0.99)),
                                     tvar99 = mean(short_term_losses$loss[short_term_losses$loss > as.numeric(quantile(short_term_losses$loss, 0.95))])
                                     )

short_term_loss_distribution = ggplot(short_term_losses, aes(x = loss)) + 
  geom_density(fill = 'steelblue', alpha = 0.6) + 
  labs(x = 'Loss', y = 'Density', title = 'Distribution of Short Term Losses') + 
  geom_vline(xintercept = short_term_loss_summary$mean, linetype = 'dashed', color = 'red') + 
  geom_vline(xintercept = short_term_loss_summary$var95, linetype = 'dashed', color = 'blue') + 
  geom_vline(xintercept = short_term_loss_summary$var99, linetype = 'dashed', color = 'brown') +
  theme_minimal() + 
  annotate('text', x = short_term_loss_summary$mean,, y = 2e-04, color = 'black', vjust = 0, angle = 90, label = paste0('Mean: ', round(short_term_loss_summary$mean))) +
  annotate('text', x = short_term_loss_summary$var95,, y = 2e-04, color = 'black', vjust = 0, angle = 90, label = paste0('VAR95: ', round(short_term_loss_summary$var95))) +
  annotate('text', x = short_term_loss_summary$var99,, y = 2e-04, color = 'black', vjust = 0, angle = 90, label = paste0('VAR99: ', round(short_term_loss_summary$var99)))






#Long Term Performance
simulate_long_term_cost = function() {
  theta = frequency_model$theta
  expected_claims = freq_pred[simulation_portfolio$worker_id %in% simulation_ids] *
  simulation_portfolio$exposure[simulation_portfolio$worker_id %in% simulation_ids]
  shape = 1 / summary(severity_model)$dispersion
  scale = sev_pred[simulation_portfolio$worker_id %in% simulation_ids] / shape
  n_policies = length(simulation_ids)
  yearly_costs = numeric(10)
  for (t in 1:10) {
    nclaims = rnbinom(n_policies, size = theta, mu = expected_claims)
    yearly_total = 0
    for (i in 1:n_policies) {
      if (nclaims[i] > 0) {
        yearly_total = yearly_total + sum(rgamma(nclaims[i], shape = shape, scale = scale[i]))
      }
    }
    yearly_costs[t] = yearly_total
  }
  yearly_costs
}


simulate_long_term_adj_cost = function(nsim = 5000) {
  cost_matrix = matrix(NA, nrow = nsim, ncol = 10)
  discount_matrix = matrix(NA, nrow = nsim, ncol = 10)
  
  for (sim in 1:nsim) {
    cost_matrix[sim, ] = simulate_long_term_cost()
    discount_matrix[sim, ] = simulate_discount()
  }
  discounted_cost_matrix = cost_matrix * discount_matrix
  return(discounted_cost_matrix)
}


long_term_discounted_costs = simulate_long_term_adj_cost()
long_term_discounted_costs = data.frame(long_term_discounted_costs)

long_term_profits_per_year = long_term_revenue_per_year - long_term_discounted_costs

expected_profit_long_term = data.frame(sim = 1:5000, expected_profit = rowSums(long_term_profits_per_year)) 
expected_long_term_losses = expected_profit_long_term %>%
  filter(expected_profit < 0)

colnames(expected_long_term_losses) = c('sim', 'loss')
expected_long_term_losses$loss = abs(expected_long_term_losses$loss)

long_term_summary = data.frame(mean = round(mean(expected_long_term_losses$loss)),
                               sd = round(sd(expected_long_term_losses$loss)), 
                               var95 = as.numeric(round(quantile(expected_long_term_losses$loss, 0.95))), 
                               var99 = as.numeric(round(quantile(expected_long_term_losses$loss, 0.99))), 
                               tvar95 = mean(expected_long_term_losses$loss[expected_long_term_losses$loss > as.numeric(round(quantile(expected_long_term_losses$loss, 0.95)))]))

long_term_summary

distribution_long_term_losses = ggplot(expected_long_term_losses, aes(x = loss)) + 
  geom_density(fill = 'grey', alpha = 0.6) + 
  theme_minimal() + 
  geom_vline(xintercept = long_term_summary$mean, color = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = long_term_summary$var95, color = 'blue', linetype = 'dashed') +
  geom_vline(xintercept = long_term_summary$var99, color = 'brown', linetype = 'dashed') + 
  labs(x = 'Loss', y = 'Density', title = 'Distribution of Long Term Simulated Losses') +
  annotate('text', x = long_term_summary$mean, y = 5e-06, label = paste0('Mean: ', long_term_summary$mean), angle = 90, vjust = 0) +
  annotate('text', x = long_term_summary$var95, y = 5e-06, label = paste0('TVAR95: ', long_term_summary$var95), angle = 90, vjust = 0) +
  annotate('text', x = long_term_summary$var99, y = 5e-06, label = paste0('TVAR99: ', long_term_summary$var99), angle = 90, vjust = 0)



grid.arrange(short_term_loss_distribution, distribution_long_term_losses, layout_matrix = rbind(c(1,1), c(2,2)))

distribution_of_long_term_profits = ggplot(expected_profit_long_term, aes(x = expected_profit)) + 
  geom_density(fill = 'violet', alpha = 0.6) + 
  theme_minimal() + 
  labs(x = 'Profit', title = 'Distribtion of Long-Term Income', y = 'Density') 

time_series_long_term_profits = ggplot(data.frame(year = 1:10, profit = colMeans(long_term_profits_per_year)), aes(x = year, y = profit)) + 
  geom_line(color = 'steelblue') + 
  geom_point(color = 'black') + 
  labs(x = 'Year', y = 'Profit', title = 'Expected Discounted Yearly Income VS Year') + 
  theme_minimal()

grid.arrange(distribution_of_long_term_profits, time_series_long_term_profits, layout_matrix = rbind(c(1,1), c(2,2)))


coef(severity_model)


#Stress Testing

#1: First Stress: Severity Tail Inflation

severity_predictions_by_worker = data.frame(worker_id = simulation_portfolio$worker_id, severity = sev_pred)

severity_stressed = function(stress_factor = 1) {
  out = severity_predictions_by_worker
  severity_95 = quantile(out$severity, 0.95)
  out$severity[out$severity > severity_95] <- out$severity[out$severity > severity_95] * stress_factor
  return(out)
}


#1a: Short-Term

simulate_stress_1_short = function(nsim = 5000, stress_factor = 1) {
  amended_expected_severity = severity_stressed(stress_factor = stress_factor)
  amended_scale = as.numeric(amended_expected_severity$severity) / shape
  sim_cost = numeric(nsim)
  for (i in 1:nsim) {
    sim_total = 0
    number_of_claims = rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    for (j in seq_along(number_of_claims)) { 
      if (number_of_claims[j] > 0) {
        sim_total = sim_total + sum(rgamma(number_of_claims[j], shape = shape, scale = amended_scale[j]))
      }
    }
    sim_cost[i] = sim_total
  }
  return(data.frame(sim = 1:nsim, cost = sim_cost))
}


sim_stress_1 = simulate_stress_1_short(stress_factor = 2)
stress_1_profit = annual_premium_revenue - sim_stress_1
colnames(stress_1_profit) = c('sim', 'profit')

ggplot(stress_1_profit, aes(x = profit)) + 
  geom_density(fill = 'green', alpha = 0.6) +
  theme_minimal() + 
  labs(x = 'Profit', y = 'Density', title = 'Distribution of Short Term Profits: Stress 1')

stress_1_loss = stress_1_profit %>%
  filter(profit < 0)
colnames(stress_1_loss) = c('sim', 'loss')

stress_1_loss$loss = abs(stress_1_loss$loss)

stress_1_loss_short_summary = data.frame(mean = round(mean(stress_1_loss$loss)), 
                                         standard_dev = round(sd(stress_1_loss$loss)),
                                         var95 = round(as.numeric(quantile(stress_1_loss$loss, 0.95))),
                                         var99 = round(as.numeric(quantile(stress_1_loss$loss, 0.99))),
                                         tvar95 = round(mean(stress_1_loss$loss[stress_1_loss$loss > as.numeric(quantile(stress_1_loss$loss, 0.99))])))

ggplot(stress_1_loss, aes(x = loss)) + 
  geom_density(fill = 'steelblue', alpha = 0.6) +
  theme_minimal() + 
  labs(x = 'Loss', y = 'Density', title = 'Distribution of Short Term Loss: Stress 1') + 
  geom_vline(xintercept = stress_1_loss_short_summary$mean, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = stress_1_loss_short_summary$var95, linetype = 'dashed', color = 'blue') + 
  geom_vline(xintercept = stress_1_loss_short_summary$var99, linetype = 'dashed', color = 'brown') +
  annotate('text', x = stress_1_loss_short_summary$mean, y = 5e-05, label = paste0('Mean: ', stress_1_loss_short_summary$mean), angle = 90, vjust = 0) +
  annotate('text', x = stress_1_loss_short_summary$var95, y = 5e-05, label = paste0('TVAR95: ', stress_1_loss_short_summary$var95), angle = 90, vjust = 0) +
  annotate('text', x = stress_1_loss_short_summary$var99, y = 5e-05, label = paste0('TVAR99: ', stress_1_loss_short_summary$var99), angle = 90, vjust = 0)



#1b: Long Term

simulate_stress_1_long_term_cost = function(nsim = 5000, stress_factor = 1) {
  amended_expected_severity = severity_stressed(stress_factor = stress_factor)
  amended_scale = as.numeric(amended_expected_severity$severity) / shape
  sim_cost = data.frame(matrix(NA, ncol = 10, nrow = nsim))
  for (i in 1:nsim) {
    sim_out = numeric(10)
    discount_rates = simulate_discount()
    for (t in 1:10) {
      yearly_cost = 0
      number_of_claims = rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
      for (j in seq_along(number_of_claims)) {
        if (number_of_claims[j] > 0) {
          yearly_cost = yearly_cost + sum(rgamma(number_of_claims[j], shape = shape, scale = amended_scale[j]))
        }
      }
      sim_out[t] = yearly_cost
    }
    sim_cost[i, ] = sim_out * discount_rates
  }
  return(sim_cost)
}

long_term_cost_stress_1 = simulate_stress_1_long_term_cost(stress_factor = 2)
colnames(long_term_cost_stress_1) = 1:10

time_series_profits_stress_1 = long_term_revenue_per_year - long_term_cost_stress_1
present_value_of_stress_1_profits = rowSums(time_series_profits_stress_1)

stress_1_long_term_distribution_of_profits = ggplot(data.frame(x = present_value_of_stress_1_profits), aes(x = x)) + 
  geom_density(fill = 'violet', alpha = 0.6) + 
  theme_minimal() + 
  labs(x = 'Profits', y = 'Density', title = 'Distribution of Stress 1 Long Term Profits')


time_series_stress_1_long_term_profits = ggplot(data.frame(year = 1:10, profit = colMeans(time_series_profits_stress_1)), aes(x = year, y = profit)) + 
  geom_line(color = 'steelblue') + 
  geom_point(color = 'black') + 
  theme_minimal() + 
  labs(x = 'Year', y = 'Profits', title = 'Expected Yearly Discounted Long Term Profits: Stress 1')

losses_stress_1 = abs(present_value_of_stress_1_profits[present_value_of_stress_1_profits < 0])

long_term_loss_summary_stress_1 = data.frame(mean = round(mean(losses_stress_1)), 
                                             standard_dev = round(sd(losses_stress_1)),
                                             var95 = round(as.numeric(quantile(losses_stress_1, 0.95))), 
                                             var99 = round(as.numeric(quantile(losses_stress_1, 0.99))), 
                                             tvar95 = round(mean(losses_stress_1[losses_stress_1 >as.numeric(quantile(losses_stress_1, 0.95))])))

distribution_long_term_losses_stress_1 = ggplot(data.frame(x = losses_stress_1), aes(x = x)) + 
  geom_density(fill = 'steelblue', alpha = 0.6) + 
  labs(x = 'Loss', y = 'Density', title = 'Distribution of Discounted Long Term Losses: Stress 1') + 
  theme_minimal() +
  geom_vline(xintercept = long_term_loss_summary_stress_1$mean, linetype = 'dashed', color = 'red') + 
  geom_vline(xintercept = long_term_loss_summary_stress_1$var95, linetype = 'dashed', color = 'blue') + 
  geom_vline(xintercept = long_term_loss_summary_stress_1$var99, linetype = 'dashed', color = 'brown') + 
  annotate('text', x = long_term_loss_summary_stress_1$mean, y = 2e-06, label = paste0('Mean: ', long_term_loss_summary_stress_1$mean), angle = 90, vjust = 0) +
  annotate('text', x = long_term_loss_summary_stress_1$var95, y = 2e-06, label = paste0('TVAR95: ', long_term_loss_summary_stress_1$var95), angle = 90, vjust = 0) +
  annotate('text', x = long_term_loss_summary_stress_1$var99, y = 2e-06, label = paste0('TVAR99: ', long_term_loss_summary_stress_1$var99), angle = 90, vjust = 0)

  


#Scenario 1
#What Extreme


simulate_s1_short <- function(nsim = 5000) {
  claim_amounts <- numeric(nsim)
  for (i in 1:nsim) {
    number_of_claims <- rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    number_of_claims[simulation_portfolio$solar_system %in% c('Epsilon', 'Zeta')] <- 1
    total_claim_amount <- 0
    for (j in seq_along(number_of_claims)) {
      if (number_of_claims[j] > 0) {
        total_claim_amount <- total_claim_amount + sum(rgamma(number_of_claims[j], shape = shape, scale = scale[j]))
      }
    }
    claim_amounts[i] <- total_claim_amount
  }
  
  return(data.frame(sim = 1:nsim, cost = claim_amounts))
}

cost_scenario_1_short = simulate_s1_short()
profit_scenario_1_short = annual_premium_revenue - cost_scenario_1_short
colnames(profit_scenario_1_short) = c('sim', 'profit')
loss_scenario_1_short = profit_scenario_1_short %>%
  filter(profit < 0)
loss_scenario_1_short$profit = abs(loss_scenario_1_short$profit)
colnames(loss_scenario_1_short) = c('sim', 'loss')

loss_summary_short_scenario_1 = data.frame(mean = round(mean(loss_scenario_1_short$loss)),
                                           sd = round(sd(loss_scenario_1_short$loss)), 
                                           var95 = round(as.numeric(quantile(loss_scenario_1_short$loss, 0.95))),
                                           var99 = round(as.numeric(quantile(loss_scenario_1_short$loss, 0.99))),
                                           tvar95 = mean(round(loss_scenario_1_short$loss[loss_scenario_1_short$loss > round(as.numeric(quantile(loss_scenario_1_short$loss, 0.95)))]))
  
)


loss_summary_short_scenario_1




#long
simulate_s1_long <- function(nsim = 5000) {
  out <- matrix(NA, nrow = nsim, ncol = 10)
  for (i in 1:nsim) {
    yearly_totals <- numeric(10)
    for (t in 1:10) {
      number_of_claims <- rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
      number_of_claims[simulation_portfolio$solar_system %in% c('Epsilon', 'Zeta')] <- 1
      yearly_total <- 0
      for (j in seq_along(number_of_claims)) {
        if (number_of_claims[j] > 0) {
          yearly_total <- yearly_total + sum(rgamma(number_of_claims[j], shape = shape, scale = scale[j]))
        }
      }
      yearly_totals[t] <- yearly_total
    }
    discount_rates <- simulate_discount()  
    out[i, ] <- yearly_totals * discount_rates
  }
  return(out)  
}

long_term_s1_costs = simulate_s1_long(nsim = 1000)
long_term_s1_profits = rowSums(long_term_revenue_per_year[1:1000, ] - long_term_s1_costs)
long_term_s1_loss = mean(long_term_s1_profits[long_term_s1_profits < 0])

#Scenario 2:

#short
simulate_s2_short <- function(nsim = 5000) {
  claim_amounts <- numeric(nsim)
  for (i in 1:nsim) {
    number_of_claims <- rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    number_of_claims[simulation_portfolio$station_id %in% c('A1', 'B1')] <- 1
    total_claim_amount <- 0
    for (j in seq_along(number_of_claims)) {
      if (number_of_claims[j] > 0) {
        total_claim_amount <- total_claim_amount + sum(rgamma(number_of_claims[j], shape = shape, scale = scale[j]))
      }
    }
    claim_amounts[i] <- total_claim_amount
  }
  
  return(data.frame(sim = 1:nsim, cost = claim_amounts))
}

cost_s2_short = simulate_s2_short()
short_term_profit = annual_premium_revenue - cost_s2_short$cost
mean(short_term_profit[short_term_profit < 0])

#Long
simulate_s2_long <- function(nsim = 5000) {
  out <- matrix(NA, nrow = nsim, ncol = 10)
  for (i in 1:nsim) {
    yearly_totals <- numeric(10)
    for (t in 1:10) {
      number_of_claims <- rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
      number_of_claims[simulation_portfolio$station_id %in% c('A1', 'B1')] <- 1
      yearly_total <- 0
      for (j in seq_along(number_of_claims)) {
        if (number_of_claims[j] > 0) {
          yearly_total <- yearly_total + sum(rgamma(number_of_claims[j], shape = shape, scale = scale[j]))
        }
      }
      yearly_totals[t] <- yearly_total
    }
    discount_rates <- simulate_discount()  
    out[i, ] <- yearly_totals * discount_rates
  }
  return(out)  
}

s2_long_costs = simulate_s2_long(nsim = 1000)

profits_s2_long = rowSums(long_term_revenue_per_year[1:1000, ] - s2_long_costs)
mean(profits_s2_long[profits_s2_long < 0])

        
#Scenario Testing
#1. What if everyone in a particular station ID made at least 1 claim (B1)
theta = frequency_model$theta
expected_claims = freq_pred * simulation_portfolio$exposure
shape = 1 / summary(severity_model)$dispersion
scale = sev_pred / shape

number_of_policy_holders = nrow(simulation_portfolio)


#Short Term
short_term_simulate_scenario_1 = function(nsim = 5000) {
  total_claims_vector = numeric(nsim)  
  for (sim in 1:nsim) {
    claims_per_policy = rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    claims_per_policy[simulation_portfolio$station_id %in% c('B1') & claims_per_policy == 0] = 1
    total_claim_amount = 0
    for (j in seq_along(claims_per_policy)) {  
      claim_count = claims_per_policy[j]
      if (claim_count > 0) {
        total_claim_amount = total_claim_amount + sum(rgamma(claim_count, shape = shape, scale = scale[j]))
      }
    }
    total_claims_vector[sim] = total_claim_amount
  }
  return(total_claims_vector)
}



short_term_cost_of_scenario_1 = short_term_simulate_scenario_1()
short_term_profits_scenario_1 = annual_premium_revenue  
short_term_scenario_1_mean = mean(short_term_profits_scenario_1)
short_term_scenario_1_tvar95 = quantile(short_term_profits_scenario_1,0.95)
short_term_scenario_1_tvar99 = quantile(short_term_profits_scenario_1,0.99)

scenario_1_short_term_density = ggplot(data.frame(x = short_term_profits_scenario_1), aes(x = x)) + 
  geom_density(color = 'black', fill = 'grey', alpha = 0.6) +
  theme_minimal() + 
  labs(x = 'Profits', y = 'Density', title = 'Distribution of Simulated Profits (Scenario 1)') +
  geom_vline(xintercept = short_term_scenario_1_mean, color = 'blue', linetype = 'dashed') +
  geom_vline(xintercept = short_term_scenario_1_tvar95, color = 'brown', linetype = 'dashed') + 
  geom_vline(xintercept = short_term_scenario_1_tvar99, color = 'red', linetype = 'dashed') +
  annotate('text', label = paste0('Mean: ', round(short_term_scenario_1_mean)), angle = 90, x = short_term_scenario_1_mean + 1000, y = 0.00005, vjust = 0, color = 'blue') +
  annotate('text', label = paste0('TVAR95: ', round(short_term_scenario_1_tvar95)), angle = 90, x = short_term_scenario_1_tvar95 + 1000, y = 0.00005, vjust = 0, color = 'brown') +
  annotate('text', label = paste0('TVAR99: ', round(short_term_scenario_1_tvar99)), angle = 90, x = short_term_scenario_1_tvar99 + 1000, y = 0.00005, vjust = 0, color = 'red')

#Long Term Moderate
#Say this happened in year 7
long_term_real_interest_rates = (1 + long_term_interest_rates)/(1+long_term_inflation_rates) - 1
long_term_cumulative_discount_rates = cumprod(1/(1+long_term_real_interest_rates))
simulate_scenario_1_long_term = long_term_profits_per_year
simulate_scenario_1_long_term$Year_7 = short_term_profits_scenario_1 * long_term_cumulative_discount_rates$X7

ggplot(data.frame(x = 1:10, y = colMeans(simulate_scenario_1_long_term)), aes(x = x, y =y)) + 
  geom_line(color = 'blue') + 
  geom_point(color = 'black') + 
  labs(x = 'Year', y = 'Profits', title = 'Expected PV of Yearly Profits Scenario 1') + 
  theme_minimal()


mean_profit_scen1 = mean(rowSums(simulate_scenario_1_long_term))
tvar95_scen1 = as.numeric(quantile(rowSums(simulate_scenario_1_long_term), 0.05))
tvar99_scen1 = as.numeric(quantile(rowSums(simulate_scenario_1_long_term), 0.01))

ggplot(data.frame(x = rowSums(simulate_scenario_1_long_term)), aes(x=x)) + 
  geom_density(color = 'black', fill = 'steelblue', alpha = 0.6) +
  theme_minimal() + 
  labs(x = 'Profits', title = 'Discounted Profits Given Scenario 1', y = 'Density') +
  geom_vline(xintercept = mean_profit_scen1, color = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = tvar95_scen1, color = 'brown', linetype = 'dashed') + 
  geom_vline(xintercept = tvar99_scen1, color = 'black', linetype = 'dashed') +
  annotate('text', label = paste0('Mean: ', round(mean_profit_scen1)), angle = 90, x = mean_profit_scen1 + 1000, y = 2e-06, vjust = 0, color = 'red') +
  annotate('text', label = paste0('TVAR95: ', round(tvar95_scen1)), angle = 90, x = tvar95_scen1 + 1000, y = 2e-06, vjust = 0, color = 'brown') +
  annotate('text', label = paste0('TVAR99: ', round(tvar99_scen1)), angle = 90, x = tvar99_scen1 + 1000, y = 2e-06,  vjust = 0, color = 'black')

#Scenario 2 Severe: What is everyone in a solar system makes at least 1 claim

#short term
simulate_scenario_2_short = function(nsim = 1000) {
  simulation_out = numeric(nsim)
  for (sim in 1:nsim) {
    claims_per_policy = rnbinom(number_of_policy_holders, size = theta, mu = expected_claims)
    claims_per_policy[simulation_portfolio$solar_system == 'Epsilon' & claims_per_policy == 0] = 1
    total_claim_amount = 0
    for (j in seq_along(claims_per_policy)) {
      if (claims_per_policy[j] > 0) {
        total_claim_amount = total_claim_amount + sum(rgamma(claims_per_policy[j], shape = shape, scale = scale[j]))
      }
    }
    simulation_out[sim] = total_claim_amount
  }
  return(simulation_out)
}

short_term_cost_scenario_2 = simulate_scenario_2_short()
short_term_profits_scenario_2 = total_1_year_premium_revenue - short_term_cost_scenario_2
short_term_profits_scenario_2 = data.frame(sim = 1:1000, profits  = short_term_profits_scenario_2)


scen_2_mean_short = mean(short_term_profits_scenario_2$profits)
scen_2_tvar95_short = quantile(short_term_profits_scenario_2$profits, 0.95)
scen_2_tvar99_short = quantile(short_term_profits_scenario_2$profits, 0.99)

ggplot(short_term_profits_scenario_2, aes(x = profits)) + 
  geom_density(color = 'black', fill = 'skyblue', alpha = 0.6) + 
  theme_minimal() + 
  labs(x = 'Profits', y = 'Density', title = 'Distribution of Short Term Profits (Scenario 2)') + 
  geom_vline(xintercept = scen_2_mean_short, color = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = scen_2_tvar95_short, color = 'blue', linetype = 'dashed') + 
  geom_vline(xintercept = scen_2_tvar99_short, color = 'brown', linetype = 'dashed') +
  annotate('text', label = paste0('Mean: ', round(scen_2_mean_short)), angle = 90, x = scen_2_mean_short + 1000, y = 2e-05, vjust = 0, color = 'red') +
  annotate('text', label = paste0('TVAR95: ', round(scen_2_tvar95_short)), angle = 90, x = scen_2_tvar95_short + 1000, y = 2e-05, vjust = 0, color = 'blue') +
  annotate('text', label = paste0('TVAR99: ', round(scen_2_tvar99_short)), angle = 90, x = scen_2_tvar99_short + 1000, y = 2e-05, vjust = 0, color = 'brown')


#long term
#say this happens in year 7


simulate_scenario_2_long_term = long_term_profits_per_year
simulate_scenario_2_long_term$Year_7 = short_term_profits_scenario_2$profits * long_term_cumulative_discount_rates$X7


ggplot(data.frame(x = 1:10, y = as.numeric(colMeans(simulate_scenario_2_long_term))), aes(x = x, y = y)) + 
  geom_line(color = 'blue') + 
  geom_point(color = 'black') + 
  theme_minimal() + 
  labs(x = 'Year', y = 'Expected Discounted Profit', title = 'Expected PV of Yearly Profits Scenario 2')

PV_scenario_2_long_term = rowSums(simulate_scenario_2_long_term)

long_term_scen_2_mean = mean(PV_scenario_2_long_term)
long_term_scen_2_tvar95 = as.numeric(quantile(PV_scenario_2_long_term, 0.05))
long_term_scen_2_tvar99 = as.numeric(quantile(PV_scenario_2_long_term, 0.01))

ggplot(data.frame(x = PV_scenario_2_long_term), aes(x = x)) + 
  geom_density(color = 'black', fill = 'violet', alpha = 0.6) +
  theme_minimal() + 
  labs(x = 'Profits', y = 'Density', title = 'Distribution of Long Term Profits (Scenario 2)') +
  geom_vline(xintercept = long_term_scen_2_mean, color = 'red', linetype = 'dashed') + 
  geom_vline(xintercept = long_term_scen_2_tvar95, color = 'blue', linetype = 'dashed') + 
  geom_vline(xintercept = long_term_scen_2_tvar99, color = 'brown', linetype = 'dashed') + 
  annotate('text', label = paste0('Mean: ', round(long_term_scen_2_mean)), angle = 90, x = long_term_scen_2_mean + 1000, y = 1e-06, vjust = 0, color = 'red') +
  annotate('text', label = paste0('TVAR95: ', round(long_term_scen_2_tvar95)), angle = 90, x = long_term_scen_2_tvar95 + 1000, y = 1e-06, vjust = 0, color = 'blue') +
  annotate('text', label = paste0('TVAR99: ', round(long_term_scen_2_tvar99)), angle = 90, x = long_term_scen_2_tvar99 + 1000, y = 1e-06, vjust = 0, color = 'brown')



