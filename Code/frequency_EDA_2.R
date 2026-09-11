library(tidyverse)
library(gridExtra)
library(effectsize)
library(rcompanion)
library(FSA)
library(corrplot)
#Station ID

ggplot(data = freq_data, mapping = aes(x = factor(station_id), fill = factor(station_id))) +
  geom_bar(color = 'black', show.legend = FALSE) + 
  theme_minimal() + 
  labs(x = 'Station ID', y = 'Count', title = 'Representation of Station ID')

ggplot(freq_data, aes(x = claim_count, fill = station_id)) +
  geom_bar(position = "fill", color = 'black') +  
  ylab("Proportion") +
  theme_minimal()

station_id_table = table(freq_data$station_id, freq_data$claim_count)
chisq.test(station_id_table)
cramerV(station_id_table)


#Solar System
ggplot(freq_data, aes(x = solar_system, fill = solar_system)) + 
  geom_bar(color = 'black') + 
  theme_minimal() + 
  labs(x = 'Solar System', y = 'Count', title = 'Representation of Solar System')

ggplot(freq_data, aes(x = claim_count, fill = solar_system)) +
  geom_bar(position = "fill", color = 'black') +  
  ylab("Proportion") + 
  labs(x = 'Claim Count', title = 'Porportion of Claim Count by Solar System')
  theme_minimal()
  
  
solar_system_data = table(freq_data$solar_system, freq_data$claim_count)
solar_system_chisq <- chisq.test(solar_system_data)
solar_system_chisq$stdres
cramerV(solar_system_data)

# ============== Occupation =================

occ1 = ggplot(freq_data, aes(x = occupation, fill = occupation)) + 
  geom_bar(color = 'black', show.legend = FALSE) + 
  theme_minimal() + 
  labs(x = 'Occupation', y = 'Count', title = 'Representation of Occupation')



occ2 = ggplot(freq_data, aes(x = claim_count, fill = occupation)) + 
  geom_bar(color = 'black', position = 'fill') + 
  theme_minimal() + 
  labs(x = 'Claim Count', y = 'Proportion', title = 'Proportion of Claim Count by Occupation', fill = 'Occupation')


grid.arrange(occ1, occ2, layout_matrix = rbind(c(1,1), c(2,2)))

occ_table = table(freq_data$occupation, freq_data$claim_count)
occ_chisqtest = chisq.test(occ_table)
occ_chisqtest$expected
occ_chisqtest$p.value
occ_chisqtest$stdres
cramerV(occ_table)


# ============== Employment Type =================

emp1 = ggplot(freq_data, aes(x = employment_type, fill = employment_type)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Employment Type', y = 'Count', fill = 'Employment Type', title = 'Representation of Employment Type') + 
  theme_minimal()

emp2 = ggplot(freq_data, aes(x = claim_count, fill = employment_type)) + 
  geom_bar(position = 'fill', color = 'black') +
  labs(y = 'Proportion', x = 'Claim Count', fill = 'Employment Type', title = 'Proportion of Claim Count by Employment Type') +
  theme_minimal()

grid.arrange(emp1, emp2, layout_matrix = rbind(c(1,1), c(2,2)))


emp_table = table(freq_data$employment_type, freq_data$claim_count)

emp_chisqtest = chisq.test(emp_table)
emp_chisqtest$p.value
cramerV(emp_table)


# ============== Accident History Flag =================
acc1 = ggplot(freq_data, aes(x = accident_history_flag, fill = accident_history_flag)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Accident History Status', y = 'Count', title = 'Representation of Accident History Status', fill = 'Accident History Flag') + 
  theme_minimal()

acc2 = ggplot(freq_data, aes(x = claim_count, fill = accident_history_flag)) + 
  geom_bar(color = 'black', position = 'fill') + 
  labs(x = 'Accident History Status', y = 'Proportion', title = 'Proportion of Claim Count by Accident History Status', fill = 'Accident History Flag') + 
  theme_minimal()

grid.arrange(acc1, acc2, layout_matrix = rbind(c(1,1), c(2,2)))


acc_table = table(freq_data$accident_history_flag, freq_data$claim_count)

acc_chisqtest = chisq.test(acc_table)
acc_chisqtest$expected
acc_chisqtest$p.value

cramerV(acc_table)


# ============== Psych Stress Index =================

psy1 = ggplot(freq_data, aes(x = psych_stress_index, fill = psych_stress_index)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Psych Stress Index', y = 'Count', title = 'Representation of Psych Stress Index', fill = 'Psych Stress Index') + 
  theme_minimal()

psy2 = ggplot(freq_data, aes(x = claim_count, fill = psych_stress_index)) + 
  geom_bar(color = 'black', position = 'fill') + 
  labs(x = 'Psych Stress Index', y = 'Proportion', title = 'Proportion of Claim Count by Psych Stress Index', fill = 'Psych Stress Index') + 
  theme_minimal()

grid.arrange(psy1, psy2, layout_matrix = rbind(c(1,1), c(2,2)))


psy_table = table(freq_data$psych_stress_index, freq_data$claim_count)

psy_chisqtest = chisq.test(psy_table)
psy_chisqtest$expected
psy_chisqtest$p.value
cramerV(psy_table)


# ============== Supervision Level =================
sup1 = ggplot(freq_data, aes(x = supervision_level, fill = supervision_level)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Supervision Level', y = 'Count', title = 'Representation of Supervision Level', fill = 'Supervision Level') + 
  theme_minimal()

sup2 = ggplot(freq_data, aes(x = claim_count, fill = supervision_level)) + 
  geom_bar(color = 'black', position = 'fill') + 
  labs(x = 'Supervision Level', y = 'Proportion', title = 'Proportion of Claim Count by Supervision Level', fill = 'Supervision Level') + 
  theme_minimal()

grid.arrange(sup1, sup2, layout_matrix = rbind(c(1,1), c(2,2)))


suptable = table(freq_data$supervision_level, freq_data$claim_count)

sup_chisqtest = chisq.test(suptable)
sup_chisqtest$expected
sup_chisqtest$p.value
cramerV(suptable)

kruskal.test(freq_data$supervision_level ~ freq_data$claim_count)

# ============== Safety Training Index =================

sti <- ggplot(freq_data, aes(x = safety_training_index, fill = safety_training_index)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Safety Training Index', 
       y = 'Count', 
       title = 'Representation of Safety Training Index', 
       fill = 'Safety Training Index') + 
  theme_minimal()

sti2 <- ggplot(freq_data, aes(x = claim_count, fill = safety_training_index)) + 
  geom_bar(color = 'black', position = 'fill') + 
  labs(x = 'Claim Count', 
       y = 'Proportion', 
       title = 'Proportion of Claim Count by Safety Training Index', 
       fill = 'Safety Training Index') + 
  theme_minimal()

grid.arrange(sti, sti2, layout_matrix = rbind(c(1,1), c(2,2)))

sti_table <- table(freq_data$safety_training_index, freq_data$claim_count)

sti_chisq <- chisq.test(sti_table)

sti_chisq$expected

sti_chisq$p.value

cramerV(sti_table)


# ============== Protective Gear Qualtiy =================
pgq_plot <- ggplot(freq_data, aes(x = protective_gear_quality, fill = protective_gear_quality)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Protective Gear Quality', 
       y = 'Count', 
       title = 'Representation of Protective Gear Quality', 
       fill = 'Protective Gear Quality') + 
  theme_minimal()

pgq_prop <- ggplot(freq_data, aes(x = claim_count, fill = protective_gear_quality)) + 
  geom_bar(color = 'black', position = 'fill') + 
  labs(x = 'Claim Count', 
       y = 'Proportion', 
       title = 'Proportion of Claim Count by Protective Gear Quality', 
       fill = 'Protective Gear Quality') + 
  theme_minimal()

grid.arrange(pgq_plot, pgq_prop, layout_matrix = rbind(c(1,1), c(2,2)))

pgq_table <- table(freq_data$protective_gear_quality, freq_data$claim_count)

pgq_chisq <- chisq.test(pgq_table)

pgq_chisq$expected
pgq_chisq$p.value
cramerV(pgq_table)

#No evidence of association


# ============== Hours Per Week =================
hpw_plot <- ggplot(freq_data, aes(x = hours_per_week, fill = hours_per_week)) + 
  geom_bar(color = 'black') + 
  labs(x = 'Hours per Week', 
       y = 'Count', 
       title = 'Representation of Hours per Week', 
       fill = 'Hours per Week') + 
  theme_minimal()

hpw_prop <- ggplot(freq_data, aes(x = claim_count, fill = hours_per_week)) + 
  geom_bar(color = 'black', position = 'fill') + 
  labs(x = 'Claim Count', 
       y = 'Proportion', 
       title = 'Proportion of Claim Count by Hours per Week', 
       fill = 'Hours per Week') + 
  theme_minimal()

grid.arrange(hpw_plot, hpw_prop, layout_matrix = rbind(c(1,1), c(2,2)))

hpw_table <- table(freq_data$hours_per_week, freq_data$claim_count)

hpw_chisq <- chisq.test(hpw_table)

hpw_chisq$expected
hpw_chisq$p.value
cramerV(hpw_table)
hpw_chisq$stdres


# ============== Exposure =================
kruskal.test(exposure ~ claim_count, data = freq_data)

dunnTest(exposure ~ as.factor(claim_count), data = freq_data, method="bonferroni")


ex1 = ggplot(freq_data, aes(x = exposure, fill = as.factor(claim_count))) + 
  geom_density(color = 'black', alpha = 0.5) + 
  theme_minimal() + 
  labs(x = 'Exposure', y = 'Density', title = 'Distribution of Exposure by Claim Count', 
       fill = 'Claim Count')

summary_stats <- freq_data %>%
  group_by(claim_count) %>%
  summarise(
    mean_exposure = mean(exposure),
    median_exposure = median(exposure),
    n = n()
  )

summary_stats

ex2 = ggplot(freq_data, aes(x=factor(claim_count), y=exposure)) +
  geom_jitter(width=0.2, alpha=0.3, color="gray") +  
  geom_point(data=summary_stats, aes(x=factor(claim_count), y=mean_exposure),
             color="red", size=4) +  
  geom_point(data=summary_stats, aes(x=factor(claim_count), y=median_exposure),
             color="blue", size=4, shape=18) + 
  labs(x="Claim Count", y="Exposure", 
       title="Exposure by Claim Count with Mean (red) & Median (blue)") +
  theme_minimal()


grid.arrange(ex1, ex2, layout_matrix = rbind(c(1,1), c(2,2)))


# ============== Gravity Level =================
kruskal.test(gravity_level ~ claim_count, data = freq_data)
dunnTest(gravity_level ~ as.factor(claim_count), data = freq_data, method="bonferroni")

grav1 = ggplot(freq_data, aes(x = gravity_level, fill = as.factor(claim_count))) + 
  geom_density(color = 'black', alpha = 0.5) + 
  theme_minimal() + 
  labs(x = 'Gravity Level', y = 'Density', title = 'Distribution of Gravity Level by Claim Count', 
       fill = 'Claim Count')

summary_stats <- freq_data %>%
  group_by(claim_count) %>%
  summarise(
    mean_gravity_level = mean(gravity_level),
    median_gravity_level = median(gravity_level),
    n = n()
  )

summary_stats

grav2 = ggplot(freq_data, aes(x=factor(claim_count), y=gravity_level)) +
  geom_jitter(width=0.2, alpha=0.3, color="gray") +  
  geom_point(data=summary_stats, aes(x=factor(claim_count), y=mean_gravity_level),
             color="red", size=4) +  
  geom_point(data=summary_stats, aes(x=factor(claim_count), y=median_gravity_level),
             color="blue", size=4, shape=18) + 
  labs(x="Claim Count", y="Gravity Level", 
       title="Gravity Level by Claim Count with Mean (red) & Median (blue)") +
  theme_minimal()


grid.arrange(grav1, grav2, layout_matrix = rbind(c(1,1), c(2,2)))


# ============== Experience Years =================
kruskal.test(experience_yrs ~ claim_count, data = freq_data)
dunnTest(experience_yrs ~ as.factor(claim_count), data = freq_data, method="bonferroni")

exp1 = ggplot(freq_data, aes(x = experience_yrs, fill = as.factor(claim_count))) + 
  geom_density(color = 'black', alpha = 0.5) + 
  theme_minimal() + 
  labs(x = 'Experience Years', y = 'Density', title = 'Distribution of Experience Years by Claim Count', 
       fill = 'Claim Count')

summary_stats_exp <- freq_data %>%
  group_by(claim_count) %>%
  summarise(
    mean_experience = mean(experience_yrs),
    median_experience = median(experience_yrs),
    n = n()
  )

summary_stats_exp

exp2 = ggplot(freq_data, aes(x=factor(claim_count), y=experience_yrs)) +
  geom_jitter(width=0.2, alpha=0.3, color="gray") +  
  geom_point(data=summary_stats_exp, aes(x=factor(claim_count), y=mean_experience),
             color="red", size=4) +  
  geom_point(data=summary_stats_exp, aes(x=factor(claim_count), y=median_experience),
             color="blue", size=4, shape=18) + 
  labs(x="Claim Count", y="Experience Years", 
       title="Experience Years by Claim Count with Mean (red) & Median (blue)") +
  theme_minimal()

grid.arrange(exp1, exp2, layout_matrix = rbind(c(1,1), c(2,2)))


# ============== Base Salary =================

kruskal.test(base_salary ~ claim_count, data = freq_data)

dunnTest(base_salary ~ as.factor(claim_count), data = freq_data, method="bonferroni")

sal1 = ggplot(freq_data, aes(x = base_salary, fill = as.factor(claim_count))) + 
  geom_density(color = 'black', alpha = 0.5) + 
  theme_minimal() + 
  labs(x = 'Base Salary', y = 'Density', title = 'Distribution of Base Salary by Claim Count', 
       fill = 'Claim Count')

summary_stats_sal <- freq_data %>%
  group_by(claim_count) %>%
  summarise(
    mean_salary = mean(base_salary),
    median_salary = median(base_salary),
    n = n()
  )

summary_stats_sal

sal2 = ggplot(freq_data, aes(x=factor(claim_count), y=base_salary)) +
  geom_jitter(width=0.2, alpha=0.3, color="gray") +  
  geom_point(data=summary_stats_sal, aes(x=factor(claim_count), y=mean_salary),
             color="red", size=4) +  
  geom_point(data=summary_stats_sal, aes(x=factor(claim_count), y=median_salary),
             color="blue", size=4, shape=18) + 
  labs(x="Claim Count", y="Base Salary", 
       title="Base Salary by Claim Count with Mean (red) & Median (blue)") +
  theme_minimal()

grid.arrange(sal1, sal2, layout_matrix = rbind(c(1,1), c(2,2)))


