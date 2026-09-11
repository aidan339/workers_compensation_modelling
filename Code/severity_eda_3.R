library(tidyverse)
library(gridExtra)
library(lsr)
library(corrplot)


# ================= Solar System =================
ss1 = ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(solar_system))) + 
  geom_density(color = 'black', alpha = 0.8) +
  theme_minimal() + 
  labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Solar System', fill = 'Solar System') 

ssdf = severity_data %>%
  group_by(solar_system) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )


ss2 = ggplot(ssdf, aes(x = solar_system, y = relativity_mean, fill = solar_system)) +
  geom_col(fill = "steelblue", color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal()

etaSquared(lm(claim_amount ~ factor(solar_system), data = severity_data))
kruskal.test(claim_amount ~ solar_system, data = severity_data)

pairwise.wilcox.test(
  severity_data$claim_amount,
  severity_data$solar_system,
  p.adjust.method = "bonferroni"
)

ss3 = ggplot(severity_data,
       aes(x = claim_amount, color = factor(solar_system))) +
  stat_ecdf(geom = "step") +
  scale_y_reverse() +
  scale_x_log10() +
  theme_minimal()

grid.arrange(ss1, ss2, ss3, layout_matrix = rbind(c(3,2), c(1,1)))

# ================= Station ID =================

sid1 = ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(station_id))) + 
  geom_density(color = 'black', alpha = 0.8) +
  theme_minimal() + 
  labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Station ID', fill = 'Station ID') 

sid = severity_data %>%
  group_by(station_id) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )

sid2 = ggplot(sid, aes(x = station_id, y = relativity_mean, fill = station_id)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(x = 'Station ID', y = 'Relativity Mean', title = 'Relativity Mean Across Station ID')

#A2, A6, A7, A9, B1, B7, B8, B9, G2 have a higher mean claim severity than the net average
#A2 especially has a higher median than industry average
sid3 = ggplot(sid, aes(x = station_id, y = relativity_median, fill = station_id)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(x = 'Station ID', y = 'Relativity Median', title = 'Relativity Median Across Station ID')

etaSquared(lm(claim_amount ~ factor(station_id), data = severity_data))
kruskal.test(claim_amount ~ station_id, data = severity_data)

grid.arrange(sid2, sid3, layout_matrix = rbind(c(1,1), c(2,2)))

# ================= Occupation =================

occ1 = ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(occupation))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + 
  labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Occupation', fill = 'Station ID') +
  theme(legend.position = 'none')

occ = severity_data %>%
  group_by(occupation) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )

occ2 = ggplot(occ, aes(x = occupation, y = relativity_mean, fill = occupation)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(x = 'Occupation', y = 'Relativity Mean', title = 'Relativity Mean Across Occupation')

occ3 = ggplot(occ, aes(x = occupation, y = relativity_median, fill = occupation)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(x = 'Occupation', y = 'Relativity Median', title = 'Relativity Median Across Occupation')

etaSquared(lm(claim_amount ~ factor(occupation), data = severity_data))
kruskal.test(claim_amount ~ occupation, data = severity_data)

pairwise.wilcox.test(
  severity_data$claim_amount,
  severity_data$occupation,
  p.adjust.method = "bonferroni"
)

occ4 = ggplot(severity_data,
             aes(x = claim_amount, color = factor(occupation))) +
  stat_ecdf(geom = "step") +
  scale_y_reverse() +
  scale_x_log10() +
  theme_minimal() + 
  labs(title = 'ECDF of Claim Amount By Occupation')

grid.arrange(occ1, occ4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(occ2, occ3, layout_matrix = rbind(c(1,1), c(2,2)))

# ================= Employment Type =================

emp1 = ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(employment_type))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + 
  labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Employment Type', fill = 'Employment Type') 

emp = severity_data %>%
  group_by(employment_type) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )

emp2 = ggplot(emp, aes(x = employment_type, y = relativity_mean, fill = employment_type)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(x = 'Employment Type', y = 'Relativity Mean', title = 'Relativity Mean Across Employment Type')

emp3 = ggplot(emp, aes(x = employment_type, y = relativity_median, fill = employment_type)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(x = 'Employment Type', y = 'Relativity Median', title = 'Relativity Median Across Employment Type')

emp4 = ggplot(severity_data,
              aes(x = claim_amount, color = factor(employment_type))) +
  stat_ecdf(geom = "step") +
  scale_y_reverse() +
  scale_x_log10() +
  theme_minimal() + 
  labs(title = 'ECDF of Claim Amount By Employment Type')


grid.arrange(emp1, emp4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(emp2, emp3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(employment_type), data = severity_data))
kruskal.test(claim_amount ~ employment_type, data = severity_data)

pairwise.wilcox.test(
  severity_data$claim_amount,
  severity_data$employment_type,
  p.adjust.method = "bonferroni"
)

# ================= Accident History Flag =================
acc1 <- ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(accident_history_flag))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + 
  labs(
    x = 'Claim Amount',
    y = 'Density',
    title = 'Distribution of Claim Amount Across Accident History',
    fill = 'Accident History'
  )

acc <- severity_data %>%
  group_by(accident_history_flag) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )

acc2 <- ggplot(acc, aes(x = accident_history_flag, y = relativity_mean, fill = accident_history_flag)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Accident History',
    y = 'Relativity Mean',
    title = 'Relativity Mean Across Accident History'
  )

acc3 <- ggplot(acc, aes(x = accident_history_flag, y = relativity_median, fill = accident_history_flag)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Accident History',
    y = 'Relativity Median',
    title = 'Relativity Median Across Accident History'
  )

acc4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(accident_history_flag))) +
  stat_ecdf(geom = "step") +
  scale_y_reverse() +
  scale_x_log10() +
  theme_minimal() + 
  labs(title = 'ECDF of Claim Amount By Accident History')

grid.arrange(acc1, acc4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(acc2, acc3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(accident_history_flag), data = severity_data))
kruskal.test(claim_amount ~ accident_history_flag, data = severity_data)

pairwise.wilcox.test(
  severity_data$claim_amount,
  severity_data$accident_history_flag,
  p.adjust.method = "bonferroni"
)


# ================= Psych Stress Index =================


psi1 <- ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(psych_stress_index))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + 
  labs(
    x = 'Claim Amount',
    y = 'Density',
    title = 'Distribution of Claim Amount Across Psych Stress Index',
    fill = 'Psych Stress Index'
  )

psi <- severity_data %>%
  group_by(psych_stress_index) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )

psi2 <- ggplot(psi, aes(x = psych_stress_index, y = relativity_mean, fill = psych_stress_index)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Psych Stress Index',
    y = 'Relativity Mean',
    title = 'Relativity Mean Across Psych Stress Index'
  )

psi3 <- ggplot(psi, aes(x = psych_stress_index, y = relativity_median, fill = psych_stress_index)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Psych Stress Index',
    y = 'Relativity Median',
    title = 'Relativity Median Across Psych Stress Index'
  )

psi4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(psych_stress_index))) +
  stat_ecdf(geom = "step") +
  scale_y_reverse() +
  scale_x_log10() +
  theme_minimal() + 
  labs(title = 'ECDF of Claim Amount By Psych Stress Index')

grid.arrange(psi1, psi4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(psi2, psi3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(psych_stress_index), data = severity_data))
kruskal.test(claim_amount ~ psych_stress_index, data = severity_data)

pairwise.wilcox.test(
  severity_data$claim_amount,
  severity_data$psych_stress_index,
  p.adjust.method = "bonferroni"
)


# ================= Hours Per Week =================

hrs1 <- ggplot(data = severity_data, mapping = aes(x = claim_amount, fill = factor(hours_per_week))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + 
  labs(
    x = 'Claim Amount',
    y = 'Density',
    title = 'Distribution of Claim Amount Across Hours per Week',
    fill = 'Hours per Week'
  )

hrs <- severity_data %>%
  group_by(hours_per_week) %>%
  summarise(
    std_dev = sd(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p90 = quantile(claim_amount, 0.9),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    n = n()
  ) %>%
  mutate(
    relativity_mean = mean_claim / mean(mean_claim),
    relativity_median = median_claim / mean(median_claim)
  )

hrs2 <- ggplot(hrs, aes(x = hours_per_week, y = relativity_mean, fill = hours_per_week)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Hours per Week',
    y = 'Relativity Mean',
    title = 'Relativity Mean Across Hours per Week'
  )

hrs3 <- ggplot(hrs, aes(x = hours_per_week, y = relativity_median, fill = hours_per_week)) +
  geom_col(color = 'black') +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  theme(legend.position = 'none') + 
  labs(
    x = 'Hours per Week',
    y = 'Relativity Median',
    title = 'Relativity Median Across Hours per Week'
  )

hrs4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(hours_per_week))) +
  stat_ecdf(geom = "step") +
  scale_y_reverse() +
  scale_x_log10() +
  theme_minimal() + 
  labs(title = 'ECDF of Claim Amount By Hours per Week')

grid.arrange(hrs1, hrs4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(hrs2, hrs3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(hours_per_week), data = severity_data))
kruskal.test(claim_amount ~ hours_per_week, data = severity_data)

pairwise.wilcox.test(
  severity_data$claim_amount,
  severity_data$hours_per_week,
  p.adjust.method = "bonferroni"
)


# ================= Safety Training Index =================
sti1 <- ggplot(severity_data, aes(x = claim_amount, fill = factor(safety_training_index))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + 
  labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Safety Training Index', fill = 'Safety Training Index')

sti <- severity_data %>%
  group_by(safety_training_index) %>%
  summarise(std_dev = sd(claim_amount),
            mean_claim = mean(claim_amount),
            median_claim = median(claim_amount),
            p90 = quantile(claim_amount, 0.9),
            p95 = quantile(claim_amount, 0.95),
            p99 = quantile(claim_amount, 0.99),
            n = n()) %>%
  mutate(relativity_mean = mean_claim / mean(mean_claim),
         relativity_median = median_claim / mean(median_claim))

sti2 <- ggplot(sti, aes(x = safety_training_index, y = relativity_mean, fill = safety_training_index)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Safety Training Index', y = 'Relativity Mean', title = 'Relativity Mean Across Safety Training Index')

sti3 <- ggplot(sti, aes(x = safety_training_index, y = relativity_median, fill = safety_training_index)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Safety Training Index', y = 'Relativity Median', title = 'Relativity Median Across Safety Training Index')

sti4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(safety_training_index))) +
  stat_ecdf(geom = "step") + scale_y_reverse() + scale_x_log10() +
  theme_minimal() + labs(title = 'ECDF of Claim Amount By Safety Training Index')

grid.arrange(sti1, sti4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(sti2, sti3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(safety_training_index), data = severity_data))
kruskal.test(claim_amount ~ safety_training_index, data = severity_data)
pairwise.wilcox.test(severity_data$claim_amount, severity_data$safety_training_index, p.adjust.method = "bonferroni")


# ================= Protective Gear Quality =================
pgq1 <- ggplot(severity_data, aes(x = claim_amount, fill = factor(protective_gear_quality))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Protective Gear Quality', fill = 'Protective Gear Quality')

pgq <- severity_data %>%
  group_by(protective_gear_quality) %>%
  summarise(std_dev = sd(claim_amount),
            mean_claim = mean(claim_amount),
            median_claim = median(claim_amount),
            p90 = quantile(claim_amount, 0.9),
            p95 = quantile(claim_amount, 0.95),
            p99 = quantile(claim_amount, 0.99),
            n = n()) %>%
  mutate(relativity_mean = mean_claim / mean(mean_claim),
         relativity_median = median_claim / mean(median_claim))

pgq2 <- ggplot(pgq, aes(x = protective_gear_quality, y = relativity_mean, fill = protective_gear_quality)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Protective Gear Quality', y = 'Relativity Mean', title = 'Relativity Mean Across Protective Gear Quality')

pgq3 <- ggplot(pgq, aes(x = protective_gear_quality, y = relativity_median, fill = protective_gear_quality)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Protective Gear Quality', y = 'Relativity Median', title = 'Relativity Median Across Protective Gear Quality')

pgq4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(protective_gear_quality))) +
  stat_ecdf(geom = "step") + scale_y_reverse() + scale_x_log10() +
  theme_minimal() + labs(title = 'ECDF of Claim Amount By Protective Gear Quality')

grid.arrange(pgq1, pgq4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(pgq2, pgq3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(protective_gear_quality), data = severity_data))
kruskal.test(claim_amount ~ protective_gear_quality, data = severity_data)
pairwise.wilcox.test(severity_data$claim_amount, severity_data$protective_gear_quality, p.adjust.method = "bonferroni")


# ================= Injury Type =================
it1 <- ggplot(severity_data, aes(x = claim_amount, fill = factor(injury_type))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Injury Type', fill = 'Injury Type')

it <- severity_data %>%
  group_by(injury_type) %>%
  summarise(std_dev = sd(claim_amount),
            mean_claim = mean(claim_amount),
            median_claim = median(claim_amount),
            p90 = quantile(claim_amount, 0.9),
            p95 = quantile(claim_amount, 0.95),
            p99 = quantile(claim_amount, 0.99),
            n = n()) %>%
  mutate(relativity_mean = mean_claim / mean(mean_claim),
         relativity_median = median_claim / mean(median_claim))

it2 <- ggplot(it, aes(x = injury_type, y = relativity_mean, fill = injury_type)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Injury Type', y = 'Relativity Mean', title = 'Relativity Mean Across Injury Type')

it3 <- ggplot(it, aes(x = injury_type, y = relativity_median, fill = injury_type)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Injury Type', y = 'Relativity Median', title = 'Relativity Median Across Injury Type')

it4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(injury_type))) +
  stat_ecdf(geom = "step") + scale_y_reverse() + scale_x_log10() +
  theme_minimal() + labs(title = 'ECDF of Claim Amount By Injury Type')

grid.arrange(it1, it4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(it2, it3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(injury_type), data = severity_data))
kruskal.test(claim_amount ~ injury_type, data = severity_data)
pairwise.wilcox.test(severity_data$claim_amount, severity_data$injury_type, p.adjust.method = "bonferroni")


# ================= Injury Cause =================
ic1 <- ggplot(severity_data, aes(x = claim_amount, fill = factor(injury_cause))) + 
  geom_density(color = 'black', alpha = 0.5) +
  theme_minimal() + labs(x = 'Claim Amount', y = 'Density', title = 'Distribution of Claim Amount Across Injury Cause', fill = 'Injury Cause')

ic <- severity_data %>%
  group_by(injury_cause) %>%
  summarise(std_dev = sd(claim_amount),
            mean_claim = mean(claim_amount),
            median_claim = median(claim_amount),
            p90 = quantile(claim_amount, 0.9),
            p95 = quantile(claim_amount, 0.95),
            p99 = quantile(claim_amount, 0.99),
            n = n()) %>%
  mutate(relativity_mean = mean_claim / mean(mean_claim),
         relativity_median = median_claim / mean(median_claim))

ic2 <- ggplot(ic, aes(x = injury_cause, y = relativity_mean, fill = injury_cause)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Injury Cause', y = 'Relativity Mean', title = 'Relativity Mean Across Injury Cause')

ic3 <- ggplot(ic, aes(x = injury_cause, y = relativity_median, fill = injury_cause)) +
  geom_col(color = 'black') + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + theme(legend.position = 'none') +
  labs(x = 'Injury Cause', y = 'Relativity Median', title = 'Relativity Median Across Injury Cause')

ic4 <- ggplot(severity_data, aes(x = claim_amount, color = factor(injury_cause))) +
  stat_ecdf(geom = "step") + scale_y_reverse() + scale_x_log10() +
  theme_minimal() + labs(title = 'ECDF of Claim Amount By Injury Cause')

grid.arrange(ic1, ic4, layout_matrix = rbind(c(1,1), c(2,2)))
grid.arrange(ic2, ic3, layout_matrix = rbind(c(1,1), c(2,2)))

etaSquared(lm(claim_amount ~ factor(injury_cause), data = severity_data))
kruskal.test(claim_amount ~ injury_cause, data = severity_data)
pairwise.wilcox.test(severity_data$claim_amount, severity_data$injury_cause, p.adjust.method = "bonferroni")


# ================= Exposure =================
e1 = ggplot(severity_data, aes(x = exposure)) +
  geom_density(alpha = 0.5, fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Density of Exposure")

e2 = ggplot(severity_data, aes(x = exposure, y = claim_amount)) +
  geom_point(alpha = 0.5, color = "#1f78b4") +
  geom_smooth(method = "loess", se = TRUE, color = "#1f78b4") +
  theme_minimal() +
  labs(title = "Claim Severity vs Exposure")

e3 = ggplot(severity_data, aes(x = exposure)) +
  geom_boxplot(alpha = 0.5, fill = "#1f78b4") +
  theme_minimal() +
  labs(title = "Boxplot of Exposure")

severity_data %>%
  summarise(std_dev = sd(exposure),
            mean_exposre = mean(exposure),
            median_exposure = median(exposure),
            p90 = quantile(exposure, 0.9),
            p95 = quantile(exposure, 0.95),
            p99 = quantile(exposure, 0.99))

grid.arrange(e1, e2, e3, layout_matrix = rbind(c(1,2), c(3,3)))

cor.test(severity_data$exposure, severity_data$claim_amount, method = "pearson")


# ================= Base Salary =================
b1 = ggplot(severity_data, aes(x = base_salary)) +
  geom_density(alpha = 0.5, fill = "#33a02c") +
  theme_minimal() +
  labs(title = "Density of Base Salary")

b2 = ggplot(severity_data, aes(x = base_salary, y = claim_amount)) +
  geom_point(alpha = 0.5, color = "#33a02c") +
  geom_smooth(method = "loess", se = TRUE, color = "#33a02c") +
  theme_minimal() +
  labs(title = "Claim Severity vs Base Salary")

b3 = ggplot(severity_data, aes(x = base_salary)) +
  geom_boxplot(alpha = 0.5, fill = "#33a02c") +
  theme_minimal() +
  labs(title = "Boxplot of Base Salary")

severity_data %>%
  summarise(std_dev = sd(base_salary),
            mean_base_salary = mean(base_salary),
            median_base_salary = median(base_salary),
            p90 = quantile(base_salary, 0.9),
            p95 = quantile(base_salary, 0.95),
            p99 = quantile(base_salary, 0.99))

grid.arrange(b1, b2, b3, layout_matrix = rbind(c(1,2), c(3,3)))


cor.test(severity_data$base_salary, severity_data$claim_amount, method = "pearson")

# ================= Gravity Level =================
g1 = ggplot(severity_data, aes(x = gravity_level)) +
  geom_density(alpha = 0.5, fill = "#e31a1c") +
  theme_minimal() +
  labs(title = "Density of Gravity Level")

g2 = ggplot(severity_data, aes(x = gravity_level, y = claim_amount)) +
  geom_point(alpha = 0.5, color = "#e31a1c") +
  geom_smooth(method = "loess", se = TRUE, color = "#e31a1c") +
  theme_minimal() +
  labs(title = "Claim Severity vs Gravity Level")

g3 = ggplot(severity_data, aes(x = gravity_level)) +
  geom_boxplot(alpha = 0.5, fill = "#e31a1c") +
  theme_minimal() +
  labs(title = "Boxplot of Gravity Level")

severity_data %>%
  summarise(std_dev = sd(gravity_level),
            mean_gravity_level = mean(gravity_level),
            median_gravity_level = median(gravity_level),
            p90 = quantile(gravity_level, 0.9),
            p95 = quantile(gravity_level, 0.95),
            p99 = quantile(gravity_level, 0.99))

grid.arrange(g1, g2, g3, layout_matrix = rbind(c(1,2), c(3,3)))

cor.test(severity_data$gravity_level, severity_data$claim_amount, method = "pearson")

# ================= Claim Length =================

c1 = ggplot(severity_data, aes(x = claim_length)) +
  geom_density(alpha = 0.5, fill = "#ff7f00") +
  theme_minimal() +
  labs(title = "Density of Claim Length")

c2 = ggplot(severity_data, aes(x = claim_length, y = claim_amount)) +
  geom_point(alpha = 0.5, color = "#ff7f00") +
  geom_smooth(method = "loess", se = TRUE, color = "#ff7f00") +
  theme_minimal() +
  labs(title = "Claim Severity vs Claim Length")

c3 = ggplot(severity_data, aes(x = claim_length)) +
  geom_boxplot(alpha = 0.5, fill = "#ff7f00") +
  theme_minimal() +
  labs(title = "Boxplot of Claim Length")

severity_data %>%
  summarise(std_dev = sd(claim_length),
            mean_claim_length = mean(claim_length),
            median_claim_length = median(claim_length),
            p90 = quantile(claim_length, 0.9),
            p95 = quantile(claim_length, 0.95),
            p99 = quantile(claim_length, 0.99))

grid.arrange(c1, c2, c3, layout_matrix = rbind(c(1,2), c(3,3)))

cor.test(severity_data$claim_length, severity_data$claim_amount, method = "pearson")

# ================= Experience Years =================
ey1 = ggplot(severity_data, aes(x = experience_yrs)) +
  geom_density(alpha = 0.5, fill = "#6a3d9a") +
  theme_minimal() +
  labs(title = "Density of Experience Years")

ey2 = ggplot(severity_data, aes(x = experience_yrs, y = claim_amount)) +
  geom_point(alpha = 0.5, color = "#6a3d9a") +
  geom_smooth(method = "loess", se = TRUE, color = "#6a3d9a") +
  theme_minimal() +
  labs(title = "Claim Severity vs Experience Years")

ey3 = ggplot(severity_data, aes(x = experience_yrs)) +
  geom_boxplot(alpha = 0.5, fill = "#6a3d9a") +
  theme_minimal() +
  labs(title = "Boxplot of Experience Years")

severity_data %>%
  summarise(std_dev = sd(experience_yrs),
            mean_experience_years = mean(experience_yrs),
            median_experience_years = median(experience_yrs),
            p90 = quantile(experience_yrs, 0.9),
            p95 = quantile(experience_yrs, 0.95),
            p99 = quantile(experience_yrs, 0.99))

grid.arrange(ey1, ey2, ey3, layout_matrix = rbind(c(1,2), c(3,3)))

cor.test(severity_data$experience_yrs, severity_data$claim_amount, method = "pearson")


numeric_vars <- severity_data[, sapply(severity_data, is.numeric)]
corr_matrix <- cor(numeric_vars, use = "complete.obs", method = "pearson")
corrplot(corr_matrix, method = "color", addCoef.col = "black")