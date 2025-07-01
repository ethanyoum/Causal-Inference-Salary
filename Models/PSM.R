data$work_setting <- as.factor(ifelse(data$work_setting == "Remote", 1, 0))  # Treatment Variable (Remote = 1, In-person = 0)
data$job_title <- as.factor(data$job_title)  # Data Analyst vs Data Engineer
data$experience_level <- as.factor(data$experience_level)  # Entry-level, Mid-level, Senior, Executive
data$company_size <- as.factor(data$company_size)  # Small (S), Medium (M), Large (L)
data$employment_type <- as.factor(data$employment_type)  # Full-time vs Contract
data$company_location <- as.factor(data$company_location)  # US locations

# Run Propensity Score Matching Model
match_out <- matchit(work_setting ~ job_title + experience_level + company_size + employment_type, 
                     data = data, method = "full", distance = "logit", ratio=2, estimand = "ATT")
summary(match_out)

# Extract Matched Data
matched_data <- match.data(match_out)

# Visualizing Balance
plot(match_out, type = "jitter", interactive = FALSE)
plot(match_out, type = "hist")

psm_effect <- lm(salary_in_usd ~ work_setting * job_title, data = matched_data)
summary(psm_effect)

t.test(matched_data$salary_in_usd ~ matched_data$work_setting)

ATT.Model <- lm(salary ~ work_setting * job_title + experience_level + company_size + 
                employment_type, data = matched_data, weights = matched_data$weights)

summary(ATT.Model)

match_out_ATC <- matchit(work_setting ~ job_title + experience_level + company_size + employment_type, 
                     data = data, method = "full", distance = "logit", ratio=2, estimand = "ATT")
summary(match_out_ATC)

matched_data_ATC <- match.data(match_out_ATC)

# Run regression on matched data (ATC)
ATC.Model <- lm(salary ~ work_setting * job_title + experience_level + 
                company_size + employment_type, 
                data = matched_data_ATC, weights = matched_data_ATC$weights)

# Display summary of the model
summary(ATC.Model)

match_out_ATE <- matchit(work_setting ~ job_title + experience_level + company_size + employment_type, 
                     data = data, method = "full", distance = "logit", ratio=2, estimand = "ATE")
summary(match_out_ATE)

matched_data_ATE <- match.data(match_out_ATE)

# Run regression on matched data (ATC)
ATE.Model <- lm(salary ~ work_setting * job_title + experience_level + 
                company_size + employment_type, 
                data = matched_data_ATC, weights = matched_data_ATE$weights)

# Display summary of the model
summary(ATE.Model)
