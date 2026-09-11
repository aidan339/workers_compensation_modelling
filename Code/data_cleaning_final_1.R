library('readxl')
library('tidyverse')

#################################Severity Data################################################
file_path <- '../Data/srcsc-2026-claims-workers-comp.xlsx'


severity_data = read_excel(file_path, sheet = 2)
#Claim_id
severity_data = severity_data[!is.na(severity_data$claim_id),]
for (j in seq_along(severity_data$claim_id)) {
  id = severity_data$claim_id[j]
  if (grepl('_', id)) {
    severity_data$claim_id = strsplit(id, '_')[[1]][1]
  }
}

#Worker_id
severity_data = severity_data[!is.na(severity_data$worker_id),]

for (j in seq_along(severity_data$worker_id)) {
  id = severity_data$worker_id[j]
  if (grepl('_', id)) {
    severity_data$worker_id[j] = strsplit(id, '_')[[1]][1]
  }
}

#Policy ID
severity_data = severity_data[!is.na(severity_data$policy_id),]
for (j in seq_along(severity_data$policy_id)) {
  id = severity_data$policy_id[j]
  if (grepl('_', id)) {
    severity_data$policy_id = strsplit(id, '_')[[1]][1]
  }
}

#solar system
severity_data = severity_data[!is.na(severity_data$solar_system),]

#station id
severity_data = severity_data[!is.na(severity_data$station_id),]
for (j in seq_along(severity_data$station_id)) {
  id = severity_data$station_id[j]
  if (grepl('_', id)) {
    severity_data$station_id[j] = strsplit(id, '_')[[1]][1]
  }
}

#Occupation
for (j in seq_along(severity_data$occupation)) {
  occupation = severity_data$occupation[j]
  if (grepl('_', occupation)) {
    severity_data$occupation[j] = strsplit(id, '_')[[1]][1]
  }
}
severity_data = severity_data[!(severity_data$occupation == 'A3'), ]


#Employment type
severity_data = severity_data[!is.na(severity_data$employment_type),]
for (j in seq_along(severity_data$employment_type)) {
  val = severity_data$employment_type[j]
  if (grepl('_', val)) {
    severity_data$employment_type[j] = strsplit(val, '_')[[1]][1]
  }
}

#Experience Years
severity_data = severity_data[!is.na(severity_data$experience_yrs),]
severity_data = severity_data[severity_data$experience_yrs <= 40, ]

#Accident History Flag
severity_data = severity_data[!is.na(severity_data$accident_history_flag), ]

#Psych
severity_data = severity_data[!is.na(severity_data$psych_stress_index),]
severity_data$psych_stress_index = abs(severity_data$psych_stress_index)
severity_data = severity_data[severity_data$psych_stress_index <= 5,]

#Hours per week
severity_data = severity_data[(!is.na(severity_data$hours_per_week) & severity_data$hours_per_week %in% c(20,25,30,35,40)),]

#Supervision level
severity_data$supervision_level = abs(severity_data$supervision_level)

#Gravity Level
severity_data = severity_data[!is.na(severity_data$gravity_level),]
severity_data$gravity_level = abs(severity_data$gravity_level)
severity_data = severity_data[severity_data$gravity_level <= 1.5, ]

#Safety Training Index
severity_data$safety_training_index = abs(severity_data$safety_training_index)
severity_data = severity_data[severity_data$safety_training_index <= 5,]

#Protective gear quality
severity_data = severity_data[!is.na(severity_data$protective_gear_quality),]

#Base Salary
severity_data = severity_data[!is.na(severity_data$base_salary),]
severity_data$base_salary = abs(severity_data$base_salary)
severity_data = severity_data[severity_data$base_salary < 130000,]

#Exposure
severity_data = severity_data[!is.na(severity_data$exposure),]
severity_data$exposure = abs(severity_data$exposure)
severity_data = severity_data[severity_data$exposure <= 1,]

#Injury Type
severity_data = severity_data[!is.na(severity_data$injury_type),]
for (j in seq_along(severity_data$injury_type)) {
  if (grepl('_', severity_data$injury_type[j])) {
    severity_data$injury_type[j] = strsplit(severity_data$injury_type[j], '_')[[1]][1]
  }
}

#Injury Cause
severity_data = severity_data[!is.na(severity_data$injury_cause),]
for (j in seq_along(severity_data$injury_cause)) {
  if (grepl('_', severity_data$injury_cause[j])) {
    severity_data$injury_cause[j] = strsplit(severity_data$injury_cause[j], '_')[[1]][1]
  }
}

#Claim Length
severity_data = severity_data[!is.na(severity_data$claim_length),]
severity_data$claim_length = abs(severity_data$claim_length)

#Claim Amount
severity_data$claim_amount = abs(severity_data$claim_amount)
severity_data = severity_data[!is.na(severity_data$claim_amount), ]
severity_data$claim_amount = severity_data$claim_amount / 100
severity_data = severity_data[severity_data$claim_amount >=5 & severity_data$claim_amount <= 170,]


severity_data$station_id = as.factor(severity_data$station_id)
severity_data$occupation = as.factor(severity_data$occupation)
severity_data$employment_type = as.factor(severity_data$employment_type)
severity_data$accident_history_flag = as.factor(severity_data$accident_history_flag)
severity_data$psych_stress_index = as.numeric(severity_data$psych_stress_index)
severity_data$hours_per_week = as.factor(severity_data$hours_per_week)
severity_data$safety_training_index = as.numeric(severity_data$safety_training_index)
severity_data$protective_gear_quality = as.factor(severity_data$protective_gear_quality)
severity_data$supervision_level = as.factor(severity_data$supervision_level)
severity_data$injury_cause = as.factor(severity_data$injury_cause)
severity_data$injury_type = as.factor(severity_data$injury_type)

#################################Frequency Data################################################

freq_data = read_excel(file_path, sheet = 1)

#solar levels
freq_data = freq_data[!is.na(freq_data$solar_system),]
required_solar_levels = c('Helionis Cluster', 'Epsilon', 'Zeta')
for (i in seq_along(freq_data$solar_system)) {
  lev = freq_data$solar_system[i]
  if (!(lev %in% required_solar_levels)) {
    freq_data$solar_system[i] = strsplit(as.character(lev), "_")[[1]][1]
  }
}

#Occupation
freq_data = freq_data[!is.na(freq_data$occupation),]
for (i in seq_along(freq_data$occupation)) {
  lev <- as.character(freq_data$occupation[i])
  if (grepl('_', lev)) {
    freq_data$occupation[i] <- strsplit(lev, "_")[[1]][1]
  }
}

#Employment Type
freq_data = freq_data[!is.na(freq_data$employment_type),]
for (i in seq_along(freq_data$employment_type)) {
  lev <- as.character(freq_data$employment_type[i])
  if (grepl('_', lev)) {
    freq_data$employment_type[i] <- strsplit(lev, "_")[[1]][1]
  }
}

#Accident History Flag
freq_data = freq_data[!is.na(freq_data$accident_history_flag),]
freq_data$accident_history_flag = abs(freq_data$accident_history_flag)
freq_data = freq_data[freq_data$accident_history_flag %in% c(0,1),]

#Psych
freq_data = freq_data[!is.na(freq_data$psych_stress_index),]
freq_data$psych_stress_index = abs(freq_data$psych_stress_index)
freq_data = freq_data[freq_data$psych_stress_index <= 5,]

#Hours per week
freq_data$hours_per_week = abs(freq_data$hours_per_week)
freq_data = freq_data[!is.na(freq_data$hours_per_week) & (freq_data$hours_per_week <= 40),]

#Supervision Level
freq_data$supervision_level = abs(freq_data$supervision_level)
freq_data = freq_data[!is.na(freq_data$supervision_level) & freq_data$supervision_level <= 1,]
freq_data$supervision_level <- round(freq_data$supervision_level, 1)

#Gravity Level
freq_data = freq_data[!is.na(freq_data$gravity_level),]
freq_data$gravity_level = abs(freq_data$gravity_level)
freq_data = freq_data[freq_data$gravity_level < 1.5,]

#Safety Index
freq_data = freq_data[!is.na(freq_data$safety_training_index),]
freq_data$safety_training_index = abs(freq_data$safety_training_index)
freq_data = freq_data[freq_data$safety_training_index <=5, ]

#Protective Gear Quality
freq_data = freq_data[!is.na(freq_data$protective_gear_quality),]
freq_data$protective_gear_quality = abs(freq_data$protective_gear_quality)
freq_data = freq_data[freq_data$protective_gear_quality <= 5, ]

#Base Salay
freq_data = freq_data[!is.na(freq_data$base_salary),]
freq_data$base_salary = abs(freq_data$base_salary)
freq_data = freq_data[freq_data$base_salary <= 130000, ]

#Exposure
freq_data = freq_data[!is.na(freq_data$exposure),]
freq_data$exposure = abs(freq_data$exposure)
freq_data = freq_data[freq_data$exposure <= 1,]

#Claim Count
freq_data = freq_data[!is.na(freq_data$claim_count),]
freq_data = freq_data[freq_data$claim_count %in% c(0,1,2),]

#Station ID
freq_data = freq_data[!is.na(freq_data$station_id),]
for (i in seq_along(freq_data$station_id)) {
  lev <- as.character(freq_data$station_id[i])
  if (grepl('_', lev)) {
    freq_data$station_id[i] <- strsplit(lev, "_")[[1]][1]
  }
}

#Experience Years
freq_data = freq_data[!is.na(freq_data$experience_yrs) & freq_data$experience_yrs <= 40,]
freq_data$experience_yrs = abs(freq_data$experience_yrs)

#Policy ID
freq_data = freq_data[!is.na(freq_data$policy_id),]


#Worker ID
freq_data = freq_data[!is.na(freq_data$worker_id),]
for (j in seq_along(freq_data$worker_id)) {
  id = freq_data$worker_id[j]
  if (grepl('_', id)) {
    freq_data$worker_id[j] = strsplit(id, '_')[[1]][1]
  }
}



freq_data$station_id = as.factor(freq_data$station_id)
freq_data$occupation = as.factor(freq_data$occupation)
freq_data$employment_type = as.factor(freq_data$employment_type)
freq_data$accident_history_flag = as.factor(freq_data$accident_history_flag)
freq_data$psych_stress_index = as.numeric(freq_data$psych_stress_index)
freq_data$hours_per_week = as.factor(freq_data$hours_per_week)
freq_data$safety_training_index = as.numeric(freq_data$safety_training_index)
freq_data$protective_gear_quality = as.factor(freq_data$protective_gear_quality)
freq_data$supervision_level = as.factor(freq_data$supervision_level)





