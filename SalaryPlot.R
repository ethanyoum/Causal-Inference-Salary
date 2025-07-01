# Load Data
data <- read.csv('Downloads/jobs_final.csv')
data <- data %>%
  mutate(
    salary_in_usd = as.numeric(salary_in_usd),
    experience_level = as.numeric(experience_level),
    treatment = as.numeric(treatment),
    company_size = as.numeric(company_size),
    job_title = as.factor(job_title)
  )

# Ensure Data Integrity
summary(data)
table(data$work_setting)

# Salary Distribution by Job Title and Work Setting
ggplot(data, aes(x = job_title, y = salary, fill = as.factor(work_setting))) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Job and Remote Status", 
       fill = "Remote Work") + theme_minimal()
