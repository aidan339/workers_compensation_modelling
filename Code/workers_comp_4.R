library(stats)
library(MASS)
library(statmod)
library(glmnet)
library(gridExtra)
library(gbm)
library(pscl)
library(randomForest)
library(tidyverse)

# ============================================ Severity ====================================================

#cor(severity_data$experience_yrs, severity_data$base_salary)
#evidence of multicollinearity, will test AIC on both, likely drop experience years as it is less significant
#to claim_amount



sev_predictors = c('psych_stress_index', 'occupation', 'safety_training_index',
                    'protective_gear_quality', 'base_salary', 'experience_yrs')

# Log-transform claim_amount
severity_data = severity_data %>%
  mutate(log_claim_amount = log(claim_amount))

colnames(severity_data)


#Distribution Fit

p = ppoints(nrow(severity_data))

# Gamma
gamma_fit = fitdistr(severity_data$claim_amount, "gamma")
gamma_theo = qgamma(p, shape = gamma_fit$estimate["shape"], rate = gamma_fit$estimate["rate"])
qqplot(gamma_theo, severity_data$claim_amount,
       main = "QQ Plot - Gamma", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")

# Exponential
exp_theo = qexp(p, rate = 1/mean(severity_data$claim_amount))
qqplot(exp_theo, severity_data$claim_amount,
       main = "QQ Plot - Exponential", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")

# Log Normal
lnorm_theo = qlnorm(p, meanlog = mean(severity_data$log_claim_amount), sdlog = sd(severity_data$log_claim_amount))
qqplot(lnorm_theo, severity_data$log_claim_amount,
       main = "QQ Plot - Lognormal", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")

# Inverse Gaussian
mu = mean(severity_data$claim_amount)
lambda = 1
invgauss_theo = qinvgauss(p, mean = mu, shape = lambda)
qqplot(invgauss_theo, severity_data$claim_amount,
       main = "QQ Plot - Inverse Gaussian", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")


#Density
ggplot(severity_data, aes(x = claim_amount)) +
  geom_density(aes(y = after_stat(density)), fill = "lightblue", alpha = 0.5) +
  stat_function(fun = function(x) dgamma(x, shape = gamma_fit$estimate["shape"], 
                                         rate = gamma_fit$estimate["rate"]),
                aes(color = "Gamma"), linewidth = 1) +
  stat_function(fun = function(x) dexp(x, rate = 1/mean(severity_data$claim_amount)),
                aes(color = "Exponential"), linewidth = 1) +
  stat_function(fun = function(x) dlnorm(x, meanlog = mean(severity_data$log_claim_amount), 
                                         sdlog = sd(severity_data$log_claim_amount)),
                aes(color = "Lognormal"), linewidth = 1) +
  stat_function(fun = function(x) dinvgauss(x, mean = mu, shape = lambda),
                aes(color = "Inverse Gaussian"), linewidth = 1) +
  labs(title = "Claim Amount Distribution with Fitted Densities",
       x = "Claim Amount", y = "Density", color = "Fitted Distribution") +
  theme_minimal()


#Data Prep
set.seed(42) 

train_idx = sample(nrow(severity_data), 0.8 * nrow(severity_data))

sev_train = severity_data[train_idx, ]
sev_test = severity_data[-train_idx, ]

y_test = sev_test$claim_amount

X_train_lc = model.matrix(log_claim_amount ~ psych_stress_index + occupation + safety_training_index +
                             protective_gear_quality + base_salary + experience_yrs,
                           data = sev_train)[, -1]

y_train_lc = sev_train$log_claim_amount

X_test = model.matrix(log_claim_amount ~ psych_stress_index + occupation + safety_training_index +
                         protective_gear_quality + base_salary + experience_yrs,
                       data = sev_test)[, -1]



#Gamma GLM
gamma_model = glm(claim_amount ~ psych_stress_index + occupation + safety_training_index +
                     protective_gear_quality + base_salary + experience_yrs + gravity_level,
                   family = Gamma(link = "log"), data = sev_train)

train_pred_gamma = predict(gamma_model, sev_train, type = 'response')
test_pred_gamma = predict(gamma_model, sev_test, type = 'response')


#Inverse Gaussian GLM
inv_gaus_model = glm(claim_amount ~ psych_stress_index + occupation + safety_training_index +
                        protective_gear_quality + base_salary + experience_yrs,
                      family = inverse.gaussian(link = "log"), data = sev_train)

train_pred_ig = predict(inv_gaus_model, sev_train, type = 'response')
test_pred_ig = predict(inv_gaus_model, sev_test, type = 'response')


#Log Normal Model
lognorm_model = lm(log(claim_amount) ~ psych_stress_index + occupation + safety_training_index +
                      protective_gear_quality + base_salary + experience_yrs,
                    data = sev_train)

train_pred_ln = exp(predict(lognorm_model, sev_train))
test_pred_ln = exp(predict(lognorm_model, sev_test))

#Assessing Residual Assumptions of the Lognormal Model

res_vs_fit = ggplot(data = data.frame(
  fitted_val = fitted(lognorm_model),
  res = residuals(lognorm_model)),
  mapping = aes(x = fitted_val, y = res)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = 'Fitted Values', y = 'Residuals',
       title = 'Residuals vs Fitted Values (Lognormal LM)') +
  theme_minimal()

res = residuals(lognorm_model)
res_dens = ggplot(data = data.frame(res = res), mapping = aes(x = res)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  stat_function(fun = function(x) dnorm(x, mean = 0, sd = sqrt(var(res))),
                color = "red", linetype = "dashed") +
  labs(title = "Residuals Density vs Normal Distribution (Lognormal LM)",
       x = "Residuals", y = "Density") +
  theme_minimal()

grid.arrange(res_vs_fit, res_dens, ncol = 2)

#Elastic Net Model 
alpha_grid = seq(0, 1, by = 0.01)
results = data.frame(alpha = numeric(), lambda_min = numeric(), cv_mse = numeric())

for (a in alpha_grid) {
  cv_fit = cv.glmnet(x = X_train_lc, y = y_train_lc, alpha = a, nfolds = 5)
  results = rbind(results, data.frame(alpha = a,
                                       lambda_min = cv_fit$lambda.min,
                                       cv_mse = min(cv_fit$cvm)))
}

best_row = results[which.min(results$cv_mse), ]
best_alpha = best_row$alpha
best_lambda = best_row$lambda_min

elastic_model = glmnet(x = X_train_lc, y = y_train_lc, alpha = best_alpha, lambda = best_lambda)

pred_log_train = predict(elastic_model, newx = X_train_lc)
pred_log_test = predict(elastic_model, newx = X_test)

sigma2 = mean((y_train_lc - pred_log_train)^2)

train_pred_el = exp(pred_log_train + sigma2 / 2)
test_pred_el = exp(pred_log_test + sigma2 / 2)


#Performance Table
perf = function(y_true, y_pred) {
  c(RMSE = sqrt(mean((y_true - y_pred)^2)),
    MAE = mean(abs(y_true - y_pred)))
}

performance = data.frame(
  Model = c("Gamma GLM", "Inverse Gaussian GLM", "Lognormal LM", "Elastic Net"),
  Train_RMSE = c(perf(sev_train$claim_amount, train_pred_gamma)[1],
                 perf(sev_train$claim_amount, train_pred_ig)[1],
                 perf(sev_train$claim_amount, train_pred_ln)[1],
                 perf(sev_train$claim_amount, train_pred_el)[1]),
  Train_MAE = c(perf(sev_train$claim_amount, train_pred_gamma)[2],
                perf(sev_train$claim_amount, train_pred_ig)[2],
                perf(sev_train$claim_amount, train_pred_ln)[2],
                perf(sev_train$claim_amount, train_pred_el)[2]),
  Test_RMSE = c(perf(y_test, test_pred_gamma)[1],
                perf(y_test, test_pred_ig)[1],
                perf(y_test, test_pred_ln)[1],
                perf(y_test, test_pred_el)[1]),
  Test_MAE = c(perf(y_test, test_pred_gamma)[2],
               perf(y_test, test_pred_ig)[2],
               perf(y_test, test_pred_ln)[2],
               perf(y_test, test_pred_el)[2])
)

print(performance)
gamma_scaled_deviance = summary(gamma_model)$deviance/summary(gamma_model)$df.residual
inv_gaus_scaled_deviance = summary(inv_gaus_model)$deviance/summary(inv_gaus_model)$df.residual


#Selecting A Gamma Distribution, Training A GBM on the Residuals
gamma_glm_residuals = residuals(gamma_model, type = 'deviance')

gbm_resid = gbm(
  formula = gamma_glm_residuals ~ psych_stress_index + occupation + safety_training_index + protective_gear_quality + 
    base_salary + experience_yrs + gravity_level,
  distribution = "gaussian",  
  data = sev_train,
  n.trees = 5000,
  interaction.depth = 4,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  bag.fraction = 0.7,
  cv.folds = 5
)

best_iter = gbm.perf(gbm_resid, method = "cv")

glm_pred = predict(gamma_model, type = "response")
gbm_pred = predict(gbm_resid, n.trees = best_iter, type = "response")

final_pred = pmax(glm_pred + gbm_pred,0)

training_rmse_gbm = perf(y_true = sev_train$claim_amount, y_pred = final_pred)

gbm_test_residuals = predict(gbm_resid, newdata = sev_test, type = 'response', n.trees = best_iter)

final_test_pred = pmax(test_pred_gamma + gbm_test_residuals,0)
perf(y_true = sev_test$claim_amount, y_pred = final_test_pred)



gamma_training_plot = ggplot(
  data.frame(x = fitted(gamma_model),
             y = residuals(gamma_model, type = "deviance")),
  aes(x = x, y = y)
) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(y = 'Deviance Residuals', x = 'Fitted Values',
       title = 'Training Performance of the Gamma Model')


inv_gaus_training_plot = ggplot(
  data.frame(x = fitted(inv_gaus_model),
             y = residuals(inv_gaus_model, type = "deviance")),
  aes(x = x, y = y)
) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(y = 'Deviance Residuals', x = 'Fitted Values',
       title = 'Training Performance of the Inverse Gaussian Model')


ln_training_plot = ggplot(
  data.frame(x = fitted(lognorm_model),
             y = residuals(lognorm_model, type = "deviance")),
  aes(x = x, y = y)
) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(y = 'Deviance Residuals', x = 'Fitted Values',
       title = 'Training Performance of the Lognormal Model')


grid.arrange(gamma_training_plot, inv_gaus_training_plot, ln_training_plot, layout_matrix = rbind(c(1,2), c(3,3)))


library(stats)
library(MASS)
library(statmod)
library(glmnet)
library(gridExtra)
library(gbm)


#cor(severity_data$experience_yrs, severity_data$base_salary)
#evidence of multicollinearity, will test AIC on both, likely drop experience years as it is less significant
#to claim_amount



sev_predictors = c('psych_stress_index', 'occupation', 'safety_training_index',
                    'protective_gear_quality', 'base_salary', 'experience_yrs', 'worker_id')

# Log-transform claim_amount
severity_data = severity_data %>%
  mutate(log_claim_amount = log(claim_amount))

colnames(severity_data)


#Distribution Fit

p = ppoints(nrow(severity_data))

# Gamma
gamma_fit = fitdistr(severity_data$claim_amount, "gamma")
gamma_theo = qgamma(p, shape = gamma_fit$estimate["shape"], rate = gamma_fit$estimate["rate"])
qqplot(gamma_theo, severity_data$claim_amount,
       main = "QQ Plot - Gamma", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")

# Exponential
exp_theo = qexp(p, rate = 1/mean(severity_data$claim_amount))
qqplot(exp_theo, severity_data$claim_amount,
       main = "QQ Plot - Exponential", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")

# Log Normal
lnorm_theo = qlnorm(p, meanlog = mean(severity_data$log_claim_amount), sdlog = sd(severity_data$log_claim_amount))
qqplot(lnorm_theo, severity_data$log_claim_amount,
       main = "QQ Plot - Lognormal", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")

# Inverse Gaussian
mu = mean(severity_data$claim_amount)
lambda = 1
invgauss_theo = qinvgauss(p, mean = mu, shape = lambda)
qqplot(invgauss_theo, severity_data$claim_amount,
       main = "QQ Plot - Inverse Gaussian", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red")


#Density
ggplot(severity_data, aes(x = claim_amount)) +
  geom_density(aes(y = after_stat(density)), fill = "lightblue", alpha = 0.5) +
  stat_function(fun = function(x) dgamma(x, shape = gamma_fit$estimate["shape"], 
                                         rate = gamma_fit$estimate["rate"]),
                aes(color = "Gamma"), linewidth = 1) +
  stat_function(fun = function(x) dexp(x, rate = 1/mean(severity_data$claim_amount)),
                aes(color = "Exponential"), linewidth = 1) +
  stat_function(fun = function(x) dlnorm(x, meanlog = mean(severity_data$log_claim_amount), 
                                         sdlog = sd(severity_data$log_claim_amount)),
                aes(color = "Lognormal"), linewidth = 1) +
  stat_function(fun = function(x) dinvgauss(x, mean = mu, shape = lambda),
                aes(color = "Inverse Gaussian"), linewidth = 1) +
  labs(title = "Claim Amount Distribution with Fitted Densities",
       x = "Claim Amount", y = "Density", color = "Fitted Distribution") +
  theme_minimal()


#Data Prep
set.seed(42) 

train_idx = sample(nrow(severity_data), 0.8 * nrow(severity_data))

sev_train = severity_data[train_idx, ]
sev_test = severity_data[-train_idx, ]

y_test = sev_test$claim_amount

X_train_lc = model.matrix(log_claim_amount ~ psych_stress_index + occupation + safety_training_index +
                             protective_gear_quality + base_salary + experience_yrs,
                           data = sev_train)[, -1]

y_train_lc = sev_train$log_claim_amount

X_test = model.matrix(log_claim_amount ~ psych_stress_index + occupation + safety_training_index +
                         protective_gear_quality + base_salary + experience_yrs,
                       data = sev_test)[, -1]



#Gamma GLM
gamma_model = glm(claim_amount ~ psych_stress_index + occupation + safety_training_index +
                     protective_gear_quality + base_salary + experience_yrs,
                   family = Gamma(link = "log"), data = sev_train)

train_pred_gamma = predict(gamma_model, sev_train, type = 'response')
test_pred_gamma = predict(gamma_model, sev_test, type = 'response')


#Inverse Gaussian GLM
inv_gaus_model = glm(claim_amount ~ psych_stress_index + occupation + safety_training_index +
                        protective_gear_quality + base_salary + experience_yrs,
                      family = inverse.gaussian(link = "log"), data = sev_train)

train_pred_ig = predict(inv_gaus_model, sev_train, type = 'response')
test_pred_ig = predict(inv_gaus_model, sev_test, type = 'response')


#Log Normal Model
lognorm_model = lm(log(claim_amount) ~ psych_stress_index + occupation + safety_training_index +
                      protective_gear_quality + base_salary + experience_yrs,
                    data = sev_train)

train_pred_ln = exp(predict(lognorm_model, sev_train))
test_pred_ln = exp(predict(lognorm_model, sev_test))

#Assessing Residual Assumptions of the Lognormal Model

res_vs_fit = ggplot(data = data.frame(
  fitted_val = fitted(lognorm_model),
  res = residuals(lognorm_model)),
  mapping = aes(x = fitted_val, y = res)) +
  geom_point(color = "blue", alpha = 0.6) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = 'Fitted Values', y = 'Residuals',
       title = 'Residuals vs Fitted Values (Lognormal LM)') +
  theme_minimal()

res = residuals(lognorm_model)
res_dens = ggplot(data = data.frame(res = res), mapping = aes(x = res)) + 
  geom_density(fill = "skyblue", alpha = 0.5) +
  stat_function(fun = function(x) dnorm(x, mean = 0, sd = sqrt(var(res))),
                color = "red", linetype = "dashed") +
  labs(title = "Residuals Density vs Normal Distribution (Lognormal LM)",
       x = "Residuals", y = "Density") +
  theme_minimal()

grid.arrange(res_vs_fit, res_dens, ncol = 2)

#Elastic Net Model 
alpha_grid = seq(0, 1, by = 0.01)
results = data.frame(alpha = numeric(), lambda_min = numeric(), cv_mse = numeric())

for (a in alpha_grid) {
  cv_fit = cv.glmnet(x = X_train_lc, y = y_train_lc, alpha = a, nfolds = 5)
  results = rbind(results, data.frame(alpha = a,
                                       lambda_min = cv_fit$lambda.min,
                                       cv_mse = min(cv_fit$cvm)))
}

best_row = results[which.min(results$cv_mse), ]
best_alpha = best_row$alpha
best_lambda = best_row$lambda_min

elastic_model = glmnet(x = X_train_lc, y = y_train_lc, alpha = best_alpha, lambda = best_lambda)

pred_log_train = predict(elastic_model, newx = X_train_lc)
pred_log_test = predict(elastic_model, newx = X_test)

sigma2 = mean((y_train_lc - pred_log_train)^2)

train_pred_el = exp(pred_log_train + sigma2 / 2)
test_pred_el = exp(pred_log_test + sigma2 / 2)


#Performance Table
perf = function(y_true, y_pred) {
  c(RMSE = sqrt(mean((y_true - y_pred)^2)),
    MAE = mean(abs(y_true - y_pred)))
}

performance = data.frame(
  Model = c("Gamma GLM", "Inverse Gaussian GLM", "Lognormal LM", "Elastic Net"),
  Train_RMSE = c(perf(sev_train$claim_amount, train_pred_gamma)[1],
                 perf(sev_train$claim_amount, train_pred_ig)[1],
                 perf(sev_train$claim_amount, train_pred_ln)[1],
                 perf(sev_train$claim_amount, train_pred_el)[1]),
  Train_MAE = c(perf(sev_train$claim_amount, train_pred_gamma)[2],
                perf(sev_train$claim_amount, train_pred_ig)[2],
                perf(sev_train$claim_amount, train_pred_ln)[2],
                perf(sev_train$claim_amount, train_pred_el)[2]),
  Test_RMSE = c(perf(y_test, test_pred_gamma)[1],
                perf(y_test, test_pred_ig)[1],
                perf(y_test, test_pred_ln)[1],
                perf(y_test, test_pred_el)[1]),
  Test_MAE = c(perf(y_test, test_pred_gamma)[2],
               perf(y_test, test_pred_ig)[2],
               perf(y_test, test_pred_ln)[2],
               perf(y_test, test_pred_el)[2])
)

print(performance)
gamma_scaled_deviance = summary(gamma_model)$deviance/summary(gamma_model)$df.residual
inv_gaus_scaled_deviance = summary(inv_gaus_model)$deviance/summary(inv_gaus_model)$df.residual


#Selecting A Gamma Distribution, Training A GBM on the Residuals
gamma_glm_residuals = residuals(gamma_model, type = 'deviance')

gbm_resid = gbm(
  formula = gamma_glm_residuals ~ psych_stress_index + occupation + safety_training_index + protective_gear_quality + 
    base_salary + experience_yrs,
  distribution = "gaussian",  
  data = sev_train,
  n.trees = 5000,
  interaction.depth = 4,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  bag.fraction = 0.7,
  cv.folds = 5
)

best_iter = gbm.perf(gbm_resid, method = "cv")

glm_pred = predict(gamma_model, type = "response")
gbm_pred = predict(gbm_resid, n.trees = best_iter, type = "response")

final_pred = pmax(glm_pred + gbm_pred,0)

training_rmse_gbm = perf(y_true = sev_train$claim_amount, y_pred = final_pred)

gbm_test_residuals = predict(gbm_resid, newdata = sev_test, type = 'response', n.trees = best_iter)

final_test_pred = pmax(test_pred_gamma + gbm_test_residuals,0)
perf(y_true = sev_test$claim_amount, y_pred = final_test_pred)



gamma_training_plot = ggplot(
  data.frame(x = fitted(gamma_model),
             y = residuals(gamma_model, type = "deviance")),
  aes(x = x, y = y)
) + 
  geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(y = 'Deviance Residuals', x = 'Fitted Values',
       title = 'Training Performance of the Gamma Model')


inv_gaus_training_plot = ggplot(
  data.frame(x = fitted(inv_gaus_model),
             y = residuals(inv_gaus_model, type = "deviance")),
  aes(x = x, y = y)
) + 
  geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(y = 'Deviance Residuals', x = 'Fitted Values',
       title = 'Training Performance of the Inverse Gaussian Model')


ln_training_plot = ggplot(
  data.frame(x = fitted(lognorm_model),
             y = residuals(lognorm_model, type = "deviance")),
  aes(x = x, y = y)
) + 
  geom_point(alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  theme_minimal() + 
  labs(y = 'Deviance Residuals', x = 'Fitted Values',
       title = 'Training Performance of the Lognormal Model')


grid.arrange(gamma_training_plot, inv_gaus_training_plot, ln_training_plot, layout_matrix = rbind(c(1,2), c(3,3)))


#Test Residuals
gamma_test_resid = sev_test$claim_amount - test_pred_gamma
inv_gaus_test_resid = sev_test$claim_amount - test_pred_ig
ln_test_resid = sev_test$claim_amount - test_pred_ln
el_test_resid = sev_test$claim_amount - as.vector(test_pred_el)

#Test Performance Graphs
gamma_test_plot = ggplot(
  data.frame(x = test_pred_gamma, y = gamma_test_resid),
  aes(x = x, y = y)
) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Fitted Values (Test)", y = "Test Residuals",
       title = "Test Residuals vs Fitted - Gamma Model")

inv_gaus_test_plot = ggplot(
  data.frame(x = test_pred_ig, y = inv_gaus_test_resid),
  aes(x = x, y = y)
) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Fitted Values (Test)", y = "Test Residuals",
       title = "Test Residuals vs Fitted - Inverse Gaussian Model")

ln_test_plot = ggplot(
  data.frame(x = test_pred_ln, y = ln_test_resid),
  aes(x = x, y = y)
) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Fitted Values (Test)", y = "Test Residuals",
       title = "Test Residuals vs Fitted - Lognormal Model")

el_test_plot = ggplot(
  data.frame(x = test_pred_el, y = el_test_resid),
  aes(x = s0, y = y)
) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Fitted Values (Test)", y = "Test Residuals",
       title = "Test Residuals vs Fitted - Elastic Net Model")

grid.arrange(gamma_test_plot, inv_gaus_test_plot, ln_test_plot, el_test_plot,
             layout_matrix = rbind(c(1,2), c(3,4)))



# ============================================ Frequency ====================================================

freq_data$claim_count = as.numeric(freq_data$claim_count)

lambda = mean(freq_data$claim_count)
var_claim_count = var(freq_data$claim_count)


#Mean and Variance are very similar, suggesting a poisson distribution

#Data prep
set.seed(42)

claim_0 = sample(which(freq_data$claim_count == 0))
claim_0_training = claim_0[1:round(0.15*length(claim_0))]

claim_1 = which(freq_data$claim_count == 1)
claim_1_training = claim_1[1:round(0.75*length(claim_1))]

claim_2 = which(freq_data$claim_count == 2)
claim_2_training = claim_2[1:round(0.75*length(claim_2))]

training_idx = c(claim_0_training, claim_1_training, claim_2_training)
predictors = c('occupation', 'solar_system', 'safety_training_index', 'psych_stress_index', 'accident_history_flag',
               'station_id', 'exposure', 'gravity_level', 'experience_yrs', 'base_salary', 'claim_count')

print(length(training_idx))
freq_training = freq_data[training_idx, predictors]
freq_test = freq_data[-training_idx, predictors]


proportion = freq_training %>%
  group_by(claim_count) %>%
  summarise(prop = n()/nrow(freq_training))

#Poison

poisson_model = glm(claim_count ~ occupation + solar_system + safety_training_index + psych_stress_index + accident_history_flag + gravity_level + 
                      experience_yrs + station_id + base_salary + offset(log(exposure)) + gravity_level, data = freq_training, family = poisson(link = "log"))

#Zero Inflated Negative Binomial


zinb_model = zeroinfl(claim_count ~ safety_training_index + psych_stress_index + accident_history_flag + gravity_level +
                        experience_yrs + offset(log(exposure)) + occupation + solar_system| safety_training_index + psych_stress_index + accident_history_flag + gravity_level +
                        experience_yrs  + solar_system,
                      data = freq_training,
                      dist = "negbin"
)

#Zero Inflated Poisson 


zip_model = zeroinfl(claim_count ~ safety_training_index + psych_stress_index + accident_history_flag + gravity_level +
                       experience_yrs + offset(log(exposure)) + occupation + solar_system| safety_training_index + psych_stress_index + accident_history_flag + gravity_level +
                       experience_yrs,
                     data = freq_training,
                     dist = "poisson"
)

#Metrics
rmse = function(y_true, y_pred) {
  result = sqrt(mean((y_true - y_pred)^2))
  return(result)         
}

mae = function(y_true, y_pred) {
  result = mean(abs((y_true - y_pred)))
  return(result)         
}


#Random Forrest with Cross Validation for mtree
k = 5  
folds = cut(seq(1, nrow(freq_training)), breaks = k, labels = FALSE)

p = ncol(freq_training) - 1  
mtry_grid = 1:p
cv_rmse = numeric(length(mtry_grid))

for (j in seq_along(mtry_grid)) {
  mtry_val = mtry_grid[j]
  fold_rmse = numeric(k)
  
  for (i in 1:k) {
    val_index = which(folds == i)
    cv_train = freq_training[-val_index, ]
    cv_val = freq_training[val_index, ]
    
    rf = randomForest(
      claim_count ~ occupation + solar_system + safety_training_index +
        psych_stress_index + accident_history_flag + gravity_level +
        experience_yrs + station_id + base_salary,
      data = cv_train,
      mtry = mtry_val,
      ntree = 10
    )
    preds = predict(rf, newdata = cv_val)
    
    fold_rmse[i] = sqrt(mean((cv_val$claim_count - preds)^2))
  }
  
  cv_rmse[j] = mean(fold_rmse)
}

best_mtry = mtry_grid[which.min(cv_rmse)]
cat("Best mtry selected by CV:", best_mtry, "\n")

plot(mtry_grid, cv_rmse, type = "b", xlab = "mtry", ylab = "CV RMSE",
     main = "Cross-validation for mtry selection")

rf_final = randomForest(
  claim_count ~ occupation + solar_system + safety_training_index +
    psych_stress_index + accident_history_flag + gravity_level +
    experience_yrs + station_id + base_salary,
  data = freq_training,
  mtry = best_mtry,
  ntree = 500,
  importance = TRUE
)

varImpPlot(rf_final, main = "Random Forest Variable Importance")

# =========================
# TRAIN PREDICTIONS
# =========================

pois_pred_t <- predict(poisson_model, newdata = freq_training, type = "response")
zip_pred_t   <- predict(zip_model,     newdata = freq_training, type = "response")
zinb_pred_t  <- predict(zinb_model,    newdata = freq_training, type = "response")
pred_train   <- predict(rf_final,      newdata = freq_training)

# TRAIN PERFORMANCE

pois_train_perf <- c(
  rmse(y_true = freq_training$claim_count, y_pred = pois_pred_t),
  mae(y_true  = freq_training$claim_count, y_pred = pois_pred_t)
)

zip_train_perf <- c(
  rmse(y_true = freq_training$claim_count, y_pred = zip_pred_t),
  mae(y_true  = freq_training$claim_count, y_pred = zip_pred_t)
)

zinb_train_perf <- c(
  rmse(y_true = freq_training$claim_count, y_pred = zinb_pred_t),
  mae(y_true  = freq_training$claim_count, y_pred = zinb_pred_t)
)

rf_train <- c(
  rmse(y_true = freq_training$claim_count, y_pred = pred_train),
  mae(y_true  = freq_training$claim_count, y_pred = pred_train)
)


# =========================
# TEST PREDICTIONS
# =========================

pois_pred <- predict(poisson_model, newdata = freq_test, type = "response")
zip_pred  <- predict(zip_model,     newdata = freq_test, type = "response")
zinb_pred <- predict(zinb_model,    newdata = freq_test, type = "response")
pred_test <- predict(rf_final,      newdata = freq_test)

# TEST PERFORMANCE

pois_test_perf <- c(
  rmse(y_true = freq_test$claim_count, y_pred = pois_pred),
  mae(y_true  = freq_test$claim_count, y_pred = pois_pred)
)

zip_test_perf <- c(
  rmse(y_true = freq_test$claim_count, y_pred = zip_pred),
  mae(y_true  = freq_test$claim_count, y_pred = zip_pred)
)

zinb_test_perf <- c(
  rmse(y_true = freq_test$claim_count, y_pred = zinb_pred),
  mae(y_true  = freq_test$claim_count, y_pred = zinb_pred)
)

rf_test <- c(
  rmse(y_true = freq_test$claim_count, y_pred = pred_test),
  mae(y_true  = freq_test$claim_count, y_pred = pred_test)
)


# =========================
# PERFORMANCE SUMMARY
# =========================

performance <- data.frame(matrix(NA, nrow = 4, ncol = 4))

colnames(performance) <- c("Poisson", "ZIP", "ZINB", "RF")
rownames(performance) <- c("Training_RMSE", "Training_MAE", "Test_RMSE", "Test_MAE")

performance$Poisson <- c(pois_train_perf, pois_test_perf)
performance$ZIP     <- c(zip_train_perf, zip_test_perf)
performance$ZINB    <- c(zinb_train_perf, zinb_test_perf)
performance$RF      <- c(rf_train, rf_test)




for (j in seq_along(severity_data$policy_id)) {
  
  split_list <- strsplit(severity_data$policy_id[j], "-", fixed = TRUE)[[1]]
  solar_of_current_entry <- severity_data$solar_system[j]
  
  if (solar_of_current_entry == "Epsilon") {
    split_list[2] <- "EPS"
  }
  
  if (solar_of_current_entry == "Zeta") {
    split_list[2] <- "ZET"
  }
  
  if (solar_of_current_entry == "Helionis Cluster") {
    split_list[2] <- "HEL"
  }
  
  severity_data$policy_id[j] <- paste(split_list, collapse = "-")
}


for (j in seq_along(freq_data$policy_id)) {
  
  split_list <- strsplit(freq_data$policy_id[j], "-", fixed = TRUE)[[1]]
  solar_of_current_entry <- freq_data$solar_system[j]
  
  if (solar_of_current_entry == "Epsilon") {
    split_list[2] <- "EPS"
  }
  
  if (solar_of_current_entry == "Zeta") {
    split_list[2] <- "ZET"
  }
  
  if (solar_of_current_entry == "Helionis Cluster") {
    split_list[2] <- "HEL"
  }
  
  freq_data$policy_id[j] <- paste(split_list, collapse = "-")
}