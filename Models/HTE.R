Y <- jobs$salary_in_usd
W <- jobs$treatment
X <- model.matrix(~ experience_level + company_size + job_title, 
                  data = jobs)[,-1]

# Full sample random forest model
fullsample.forest <- causal_forest(X,Y,W)
summary(fullsample.forest)

# HTE estimate
HTE.estimate <- predict(fullsample.forest)$predictions
HTE.estimate

# Compute ATE
fullsample.ATE <- average_treatment_effect(fullsample.forest)
fullsample.ATE

## Estimate is -1985.684, and std error is 1681.228.

# Compute feature importance
fullsample.varimp <- variable_importance(fullsample.forest)
ranked.vars <- order(fullsample.varimp, decreasing = TRUE)
fullsample.varimp
ranked.vars

## S-Learner
# New column time_period
jobs <- jobs %>%
  mutate(time_period = ifelse(work_year %in% c(2020, 2021, 2022), 0, 1))

X.S <- jobs %>% 
  select(experience_level, company_size, job_category, time_period)

S.model <- randomForest(cbind(W, X.S), Y, ntree = 500)

## Compute HTE estimate
# Before workforce stabilization, Analyst
X.Analyst.Before.Remote <- X.S %>% mutate(W = 1, job_title = "Data Analyst", 
                                        time_period = 0)
X.Analyst.Before.InPerson <- X.S %>% mutate(W = 0, job_title = "Data Analyst", 
                                          time_period = 0)

# After workforce stabilization, Analyst
X.Analyst.After.Remote <- X.S %>% mutate(W = 1, job_title = "Data Analyst", 
                                         time_period = 1)
X.Analyst.After.InPerson <- X.S %>% mutate(W = 0, job_title = "Data Analyst", 
                                           time_period = 1)

# Before workforce stabilization, Engineer
X.Engineer.Before.Remote <- X.S %>% mutate(W = 1, job_title = "Data Engineer", 
                                          time_period = 0)
X.Engineer.Before.InPerson <- X.S %>% mutate(W = 0, job_title = "Data Engineer", 
                                            time_period = 0)

# After workforce stabilization, Engineer
X.Engineer.After.Remote <- X.S %>% mutate(W = 1, job_title = "Data Engineer", 
                                         time_period = 1)
X.Engineer.After.InPerson <- X.S %>% mutate(W = 0, job_title = "Data Engineer", 
                                           time_period = 1)

# Predict salaries for each
Y1.Analyst.Before <- predict(S.model, X.Analyst.Before.Remote)
Y0.Analyst.Before <- predict(S.model, X.Analyst.Before.InPerson)

Y1.Analyst.After <- predict(S.model, X.Analyst.After.Remote)
Y0.Analyst.After <- predict(S.model, X.Analyst.After.InPerson)

Y1.Engineer.Before <- predict(S.model, X.Engineer.Before.Remote)
Y0.Engineer.Before <- predict(S.model, X.Engineer.Before.InPerson)

Y1.Engineer.After <- predict(S.model, X.Engineer.After.Remote)
Y0.Engineer.After <- predict(S.model, X.Engineer.After.InPerson)

# Compute HTE
HTE.Analyst.Before <- Y1.Analyst.Before - Y0.Analyst.Before
HTE.Analyst.Before

HTE.Analyst.After <- Y1.Analyst.After - Y0.Analyst.After
HTE.Analyst.After

HTE.Engineer.Before <- Y1.Engineer.Before - Y0.Engineer.Before
HTE.Engineer.Before

HTE.Engineer.After <- Y1.Engineer.After - Y0.Engineer.After
HTE.Engineer.After
