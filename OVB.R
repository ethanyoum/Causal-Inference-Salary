# Short regression (without confounder)
Model.short <- lm(salary ~ work_setting, data = data)
summary(Model.short)

sum(is.na(data$experience_level))  # Count of NA values
sum(is.nan(data$experience_level)) # Count of NaN values
sum(is.infinite(data$experience_level)) # Count of Inf values

# Auxiliary regression (confounder on work_setting)
data_clean <- na.omit(data) 
data$experience_level <- as.numeric(as.factor(data$experience_level))

Model.aux <- lm(experience_level ~ work_setting, data = data)
summary(Model.aux)

data$experience_level <- as.factor(data$experience_level)  # Entry-level, Mid-level, Senior, Executive

Model.long<-lm(salary ~ work_setting + experience_level, data = data)
summary(Model.long)

Gamma.long = Model.long$coefficients[3]
Gamma.long

Pi1 = Model.aux$coefficients[2]
Pi1

Bias.computed = Gamma.long*pi1
Bias.computed

percentage_bias <- (Bias.computed / mean(data$salary, na.rm = TRUE)) * 100
percentage_bias
