## DiD (Difference-in-Difference)

# Before US workforce stabilization
remote.before_stab <- jobs %>%
  filter(work_year %in% c(2020, 2021, 2022), work_setting == 'Remote') %>%
  select(work_year, salary_in_usd)

inperson.before_stab <- jobs %>%
  filter(work_year %in% c(2020, 2021, 2022), work_setting == 'In-person') %>%
  select(work_year, salary_in_usd)

# After US workforce stabilization
remote.after_stab <- jobs %>%
  filter(work_year == 2023, work_setting == 'Remote') %>%
  select(salary_in_usd)

inperson.after_stab <- jobs %>%
  filter(work_year == 2023, work_setting == 'In-person') %>%
  select(salary_in_usd)

# Tried 2*2 DiD if possible, but it's impossible due to discrepancy in number of obs

# Compute mean salary before stabilization
mean.remote_before <- remote.before_stab %>% summarise(mean_salary 
                                                      = mean(salary_in_usd, na.rm = TRUE))
mean.inperson_before <- inperson.before_stab %>% summarise(mean_salary 
                                                      = mean(salary_in_usd, na.rm = TRUE))

# Compute mean salary after stabilization
mean.remote_after <- remote.after_stab %>% summarise(mean_salary 
                                                     = mean(salary_in_usd, na.rm = TRUE))
mean.inperson_after <- inperson.after_stab %>% summarise(mean_salary 
                                                         = mean(salary_in_usd, na.rm = TRUE))

# ATE estimate
ATE.DiD <- (mean.remote_after$mean_salary - mean.remote_before$mean_salary) - 
  (mean.inperson_after$mean_salary - mean.inperson_before$mean_salary)
ATE.DiD

# ATT estimate
ATT.DiD <- (mean.remote_after$mean_salary - mean.remote_before$mean_salary)
ATT.DiD

# ATC estimate
ATC.DiD <- (mean.inperson_after$mean_salary - mean.inperson_before$mean_salary)
ATC.DiD

# Define unique entity ID to run pdim
jobs$unique_id <- paste(jobs$company_size, jobs$job_title, sep = "_")

# 
jobs %>%
  group_by(unique_id, work_year) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  arrange(desc(count))

jobs <- jobs %>%
  group_by(unique_id, work_year) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

# Define unique entity ID to run pdim
#jobs$unique_id <- paste(jobs$company_size, jobs$job_title, sep = "_")

# Convert to panel data
jobs_panel <- pdata.frame(jobs, index = c("unique_id", "work_year"), drop.index = TRUE, row.names = FALSE)

# Check panel dimensions
pdim(jobs_panel)

# Set time variables and treatment variable according to context
jobs <- jobs %>%
  mutate(
    time = ifelse(work_year %in% c(2020, 2021, 2022), 0, 1),
    treatment = ifelse(work_setting == "Remote", 1, 0)
  )

# Run DiD model, using FE to control for experience level and comapny size
DiD.Model <- plm(
  salary_in_usd ~ treatment * time + experience_level + company_size,
  data = jobs_panel, model = "within", effect = "individual")

summary(DiD.Model)


## ATE = -5136.8, but statistically insignificant. 
## No strong evidence that remote work changed salary

# FE to also control time-specific effects (time-fixed)
DiD.FE.Time <- plm(
  salary_in_usd ~ treatment * time + experience_level + company_size, 
  data = jobs_panel, model = "within", effect = "twoways")

# Test if time-fixed effects are useful
pFtest(DiD.FE.Time, DiD.Model)

## Time-fixed effects are not useful (p-value is 0.6031)

# Pooled OLS
DiD.Pooled <- lm(salary_in_usd ~ treatment * time + experience_level 
                 + company_size, data = jobs)

# Then check if DiD.model is useful
pFtest(DiD.Model, DiD.Pooled)
