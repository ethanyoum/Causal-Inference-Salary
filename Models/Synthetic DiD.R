# Prepare data for synthetic DiD
Y <- data %>%
  select(salary_in_usd) %>%  # Outcome variable
  as.matrix()

Y <- matrix(data$salary_in_usd, nrow = length(unique(data$work_year)), byrow = TRUE)
print(dim(Y))

N0 <- sum(data$treatment == 0)  # Number of control units
N1 <- sum(data$treatment == 1)  # Number of treated units
T0 <- sum(data$post_covid == 0) # Pre-treatment period
T1 <- sum(data$post_covid == 1) # Post-treatment period

library(synthdid)
library(tidyverse)
library(ggplot2)

data$Actual_work_setting <- as.factor(data$Actual_work_setting)
unique(data$Actual_work_setting)

# Ensure work_year is numeric
data$work_year <- as.numeric(data$work_year)

# Filter only Data Engineers and Data Analysts
data_filtered <- data %>%
  filter(job_title %in% c("Data Engineer", "Data Analyst")) %>%
  group_by(job_title, Actual_work_setting, work_year) %>%
  summarise(salary_in_usd = mean(salary_in_usd, na.rm = TRUE), .groups = "drop")

# Check the data structure after transformation
print(data_filtered)

# Aggregate salaries by mean to ensure unique (job_title, work_setting, work_year) pairs**
jobs_agg <- data %>%
  group_by(job_title, Actual_work_setting, work_year) %>%
  summarise(salary_in_usd = mean(salary_in_usd, na.rm = TRUE), .groups = "drop")

# Create Post-Stabilization Variable**
jobs_agg <- jobs_agg %>%
  mutate(post_stabilization = ifelse(work_year >= 2023, 1, 0))

# Ensure experience_level is a factor
jobs_agg$experience_level <- as.factor(jobs_agg$experience_level)

# Run Difference-in-Differences (DiD) Regression**
DiD.model <- lm(salary_in_usd ~ Actual_work_setting * post_stabilization + job_title, data = jobs_agg)

# Print regression results
summary(DiD.model)
