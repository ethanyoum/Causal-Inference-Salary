# Short regression (without confounder)
Model.Short <- lm(salary ~ work_setting, data = data)
summary(Model.Short)

sum(is.na(data$experience_level))  # Count of NA values
sum(is.nan(data$experience_level)) # Count of NaN values
sum(is.infinite(data$experience_level)) # Count of Inf values

# Auxiliary regression (confounder on work_setting)
data_clean <- na.omit(data) 
data$experience_level <- as.numeric(as.factor(data$experience_level))

Model.Aux <- lm(experience_level ~ work_setting, data = data)
summary(Model.Aux)

data$experience_level <- as.factor(data$experience_level)  # Entry-level, Mid-level, Senior, Executive

Model.Long<-lm(salary ~ work_setting + experience_level, data = data)
summary(Model.Long)

Gamma.Long = Model.Long$coefficients[3]
Gamma.Long

Pi1 = Model.Aux$coefficients[2]
Pi1

Bias.Computed = Gamma.Long*pi1
Bias.Computed

Percentage.Bias <- (Bias.Computed / mean(data$salary, na.rm = TRUE)) * 100
Percentage.Bias
