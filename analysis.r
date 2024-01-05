data_total = read.table("application_data_preprocessed.csv", header=TRUE, sep=",")
attach(data)

names(data)

data_complete = read.table("application_data_remove_na.csv", header=TRUE, sep=",")
attach(data_complete)

#STARTING OPERATIONS AND CLEANING OF DATA:
#"FLAG_OWN_CAR"
to_keep = c("CODE_GENDER", "FLAG_OWN_REALTY", "CNT_CHILDREN", "AMT_INCOME_TOTAL", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "REGION_POPULATION_RELATIVE", "DAYS_BIRTH", "DAYS_EMPLOYED", "OWN_CAR_AGE", "OCCUPATION_TYPE", "CNT_FAM_MEMBERS", "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY", "ORGANIZATION_TYPE", "EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3", "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BEGINEXPLUATATION_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ELEVATORS_AVG", "ENTRANCES_AVG", "FLOORSMAX_AVG", "FLOORSMIN_AVG", "LANDAREA_AVG", "LIVINGAPARTMENTS_AVG", "NONLIVINGAPARTMENTS_AVG", "NONLIVINGAREA_AVG", "APARTMENTS_MODE", "BASEMENTAREA_MODE", "YEARS_BEGINEXPLUATATION_MODE", "YEARS_BUILD_MODE", "COMMONAREA_MODE", "ELEVATORS_MODE", "ENTRANCES_MODE", "FLOORSMAX_MODE", "FLOORSMIN_MODE", "LANDAREA_MODE", "LIVINGAPARTMENTS_MODE", "LIVINGAREA_MODE", "NONLIVINGAPARTMENTS_MODE", "NONLIVINGAREA_MODE", "APARTMENTS_MEDI", "BASEMENTAREA_MEDI", "YEARS_BEGINEXPLUATATION_MEDI", "YEARS_BUILD_MEDI", "COMMONAREA_MEDI", "ELEVATORS_MEDI", "ENTRANCES_MEDI", "FLOORSMAX_MEDI", "FLOORSMIN_MEDI", "LANDAREA_MEDI", "LIVINGAPARTMENTS_MEDI", "LIVINGAREA_MEDI", "NONLIVINGAPARTMENTS_MEDI", "NONLIVINGAREA_MEDI", "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "TOTALAREA_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE", "OBS_30_CNT_SOCIAL_CIRCLE", "DEF_30_CNT_SOCIAL_CIRCLE", "OBS_60_CNT_SOCIAL_CIRCLE", "DEF_60_CNT_SOCIAL_CIRCLE")
categorical_features = c("CODE_GENDER", "FLAG_OWN_REALTY", "CNT_CHILDREN", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "CNT_FAM_MEMBERS", "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY", "ORGANIZATION_TYPE", "OCCUPATION_TYPE", "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE")
data = data_complete[, to_keep]
data$YEARS_BIRTH <- -data$DAYS_BIRTH/365
data$DAYS_BIRTH <- NULL
data$MONTHS_EMPLOYED <- -data$DAYS_EMPLOYED*12/365
data$DAYS_EMPLOYED <- NULL
for (feature_name in categorical_features) {
  data[[feature_name]] <- factor(data[[feature_name]])
}

#GENERATE DIFFERENT TRANSFORMATIONS FOR CONT VARS
# List of transformation functions
funcs <- list(
#  function(x) log(x),
  function(x) log(x + 1),
  exp, sqrt, 
  function(x) x^2, 
  function(x) x,
  function(x) x^3,
  function(x) 1/(x+1),
  function(x) 1/(x^2 + 1),
  function(x) x^(1/3)
)
fnames <- c("log", "exp", "sqrt", "squared", "identity", "cubed", "inverse", "inverse_squared", "cubic_root")

# Apply transformations to each column in the dataset
transformed_data <- lapply(names(data), function(col) {
  col_data <- data[[col]]
  if (col != "AMT_INCOME_TOTAL" && !is.factor(col_data)) {
    lapply(funcs, function(f) f(col_data))
  } else {
    list(col_data)  # Skip transformations for AMT_INCOME_TOTAL or if it's a factor, return as a list
  }
})

# Naming the new columns and creating a new dataframe
transformed_data_df <- as.data.frame(transformed_data)
names(transformed_data_df) <- unlist(lapply(names(data), function(col) {
  col_data <- data[[col]]
  if (col != "AMT_INCOME_TOTAL" && !is.factor(col_data)) {
    paste(rep(col, each = length(fnames)), fnames, sep = "_")
  } else {
    col  # Skip naming transformations for AMT_INCOME_TOTAL or if it's a factor, keep its original name
  }
}))

transformed_data_df

#STARTING ANALYSIS:
big_lm = lm(AMT_INCOME_TOTAL ~ ., data=data)
?lm
summary(big_lm)
qqnorm(residuals(big_lm))
qqline(residuals(big_lm))

summary(data)
summary(big_lm)$coefficients[,4]["(Intercept)"]
big_lm_p_values <- summary(big_lm)$coefficients[,4][summary(big_lm)$coefficients[,4] < 0.05]

plot(data$CODE_GENDER, data$AMT_INCOME_TOTAL)
plot(data$AMT_INCOME_TOTAL)
hist(log(data$AMT_INCOME_TOTAL))# Predicting the log of the income increases the R^2
ln_big_lm = lm(log(AMT_INCOME_TOTAL) ~ ., data=data)
summary(ln_big_lm)
qqnorm(residuals(ln_big_lm))
qqline(residuals(ln_big_lm))
ln_big_lm_p_values <- summary(ln_big_lm)$coefficients[,4][summary(big_lm)$coefficients[,4] < 0.05]

identical(big_lm_p_values, ln_big_lm_p_values)
ks.test(x=log(AMT_INCOME_TOTAL),y='pnorm',alternative='two.sided')
shapiro.test(sample(log(AMT_INCOME_TOTAL), 5000))

summary(data)
ln_big_lm_p_values

cor(data[, setdiff(names(data), categorical_features)])
t = cor(log(data$AMT_INCOME_TOTAL), data[, setdiff(names(data), union(categorical_features, "AMT_INCOME_TOTAL"))])
max(t)
t[, which.max(t)]
plot(data$REGION_POPULATION_RELATIVE, log(data$AMT_INCOME_TOTAL))
plot(log(data$AMT_INCOME_TOTAL), ln_big_lm$residuals)
test = lm(ln_big_lm$residuals ~ log(data$AMT_INCOME_TOTAL))
abline(test, col='blue')
plot(ln_big_lm$residuals)

qqnorm(log(data$AMT_INCOME_TOTAL))
qqline(log(data$AMT_INCOME_TOTAL))


#STEP-UP PROCEDURE FOR MODEL SELECTION:
step_up <- function(data) {
  important_features <- c()
  iteration_number = 0
  remaining_features <- setdiff(names(data), "AMT_INCOME_TOTAL")
  while (TRUE) {
    #print(important_features)
    iteration_number = iteration_number + 1
    print(iteration_number)
    min_p = 1
    feature_argmin <- NULL
    for (feature in remaining_features) {
      lm = lm(log(AMT_INCOME_TOTAL) ~ ., data=data[union(feature, important_features)])
      if (feature %in% categorical_features) {
        temp = names(summary(lm)$coefficients[,4])
        p_value = min(summary(lm)$coefficients[,4][temp[startsWith(temp, feature)]])
      }
      else {
        p_value = summary(lm)$coefficients[,4][feature]
      }
      #print(feature)
      #print(p_value)
      if (p_value < min_p) {
        min_p = p_value
        feature_argmin = feature
      }
    }
    print(feature_argmin)
    if (min_p < 0.05) {
      important_features <- union(important_features, feature_argmin)
    } else {
      break
    }
    remaining_features <- setdiff(remaining_features, feature_argmin)
  }
  return(important_features)
}




#Features we chose:
test <- c(
  "MONTHS_EMPLOYED_exp",
  "REGION_RATING_CLIENT_W_CITY",
  "CODE_GENDER",
  "EXT_SOURCE_1_exp",
  "NAME_INCOME_TYPE",
  "OWN_CAR_AGE_log",
  "OCCUPATION_TYPE",
  "FLOORSMAX_AVG_inverse_squared",
  "EXT_SOURCE_3_exp",
  "REGION_RATING_CLIENT",
  "YEARS_BIRTH_inverse_squared",
  "NAME_FAMILY_STATUS",
  "MONTHS_EMPLOYED_log",
  "REGION_POPULATION_RELATIVE_cubed",
  "REGION_POPULATION_RELATIVE_cubic_root",
  "EXT_SOURCE_2_inverse",
  "REGION_POPULATION_RELATIVE_sqrt",
  "CNT_FAM_MEMBERS",
  "OWN_CAR_AGE_cubic_root",
  "OWN_CAR_AGE_inverse_squared",
  "ORGANIZATION_TYPE",
  "NAME_EDUCATION_TYPE",
  "FLOORSMIN_MEDI_exp",
  "CNT_CHILDREN",
  "NONLIVINGAREA_MODE_exp",
  "YEARS_BUILD_MODE_cubic_root",
  "YEARS_BUILD_MEDI_cubed",
  "MONTHS_EMPLOYED_identity"
)

detach()
#### USING THE FULL ALLOWABLE DATASET AT OUR DISPOSAL TO RUN THE LM:
total_data <- read.table("application_data_with_important_features.csv", header=TRUE, sep=",")
attach(total_data)
names(total_data)
for (feature_name in names(total_data)) {
  if (feature_name %in% categorical_features) {
    total_data[[feature_name]] <- factor(total_data[[feature_name]])
  }
}

#transformed_data_df <- c()
#Adding transformation:
transformed_data <- lapply(names(total_data), function(col) {
  col_data <- total_data[[col]]
  if (col != "AMT_INCOME_TOTAL" && !is.factor(col_data)) {
    lapply(funcs, function(f) f(col_data))
  } else {
    list(col_data)  # Skip transformations for AMT_INCOME_TOTAL or if it's a factor, return as a list
  }
})

transformed_data_df <- as.data.frame(transformed_data)
names(transformed_data_df) <- unlist(lapply(names(total_data), function(col) {
  col_data <- total_data[[col]]
  if (col != "AMT_INCOME_TOTAL" && !is.factor(col_data)) {
    paste(rep(col, each = length(fnames)), fnames, sep = "_")
  } else {
    col  # Skip naming transformations for AMT_INCOME_TOTAL or if it's a factor, keep its original name
  }
}))

transformed_data_df[test]

lm_test = lm(log(AMT_INCOME_TOTAL) ~ ., transformed_data_df[test])
summary(lm_test)


    
#### NORMALIZING FUNCTION:
normalize_dataset <- function(dataset) {
  normalized_dataset <- dataset
  for (feature_name in names(dataset)) {
    if (feature_name %in% setdiff(names(dataset), categorical_features)) {
      normalized_dataset[[feature_name]] <- (dataset[[feature_name]] - mean(dataset[[feature_name]]))/sd(dataset[[feature_name]])
    }
  }
  return(normalized_dataset)
}

normalized_dataset <- normalize_dataset(transformed_data_df)

library(modelr)
library(purrr)
library(tidyverse)
library(plyr)


get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

summary(lm(log(AMT_INCOME_TOTAL) ~ ., data=normalized_dataset[test], na.action=na.omit))
summary(lm(AMT_INCOME_TOTAL ~ ., data=normalized_dataset[test], na.action=na.omit))

for (k in 2:10) {
  print(k)
  
  cv  <- crossv_kfold(transformed_data_df[test], k = k)
  cvn  <- crossv_kfold(normalized_dataset[test], k = k)

  models1 <- map(cv$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=transformed_data_df[test]))
  models2 <- map(cv$train, ~lm(AMT_INCOME_TOTAL ~ ., data=transformed_data_df[test]))

  modelsn1 <- map(cvn$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=normalized_dataset[test]))
  modelsn2 <- map(cvn$train, ~lm(AMT_INCOME_TOTAL ~ ., data=normalized_dataset[test]))

  pred1  <- map2_df(models1, cv$test, get_pred, .id = "Run1")
  pred2  <- map2_df(models2, cv$test, get_pred, .id = "Run2")
  
  predn1  <- map2_df(modelsn1, cvn$test, get_pred, .id = "Run1")
  predn2  <- map2_df(modelsn2, cvn$test, get_pred, .id = "Run2")

  MSE1  <- pred1 %>% group_by(Run1) %>%
    summarise(MSE = mean( (log(AMT_INCOME_TOTAL) - pred)^2))
  print(MSE1)
  print(MSE1 / mean(log(AMT_INCOME_TOTAL)))

  MSE2  <- pred2 %>% group_by(Run2) %>%
    summarise(MSE = mean( (AMT_INCOME_TOTAL - pred)^2))
  print(MSE2)
  print(MSE2 / mean(AMT_INCOME_TOTAL))

  MSEn1  <- predn1 %>% group_by(Run1) %>%
    summarise(MSE = mean( (log(AMT_INCOME_TOTAL) - pred)^2))
  print(MSEn1)
  print(MSEn1 / mean(log(AMT_INCOME_TOTAL)))

  MSEn2  <- predn2 %>% group_by(Run2) %>%
    summarise(MSE = mean( (AMT_INCOME_TOTAL - pred)^2))
  print(MSEn2)
  print(MSEn2 / mean(AMT_INCOME_TOTAL))
}


x = data.matrix(total_data[,!names(total_data) %in% c("AMT_INCOME_TOTAL")])
y = total_data$AMT_INCOME_TOTAL


x = data.matrix(transformed_data_df[,!names(transformed_data_df) %in% c("AMT_INCOME_TOTAL")])
y = log(transformed_data_df$AMT_INCOME_TOTAL)


x = data.matrix(transformed_data_df[,test])
y = log(transformed_data_df$AMT_INCOME_TOTAL)

x = data.matrix(normalized_dataset)
y = log(transformed_data_df$AMT_INCOME_TOTAL)

library(glmnet)

cv_model <- cv.glmnet(x, y, alpha=1)

best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq



res_aov <- aov(AMT_INCOME_TOTAL ~ CODE_GENDER + OCCUPATION_TYPE + CODE_GENDER:OCCUPATION_TYPE, data = transformed_data_df)
summary(res_aov)
qqnorm(residuals(res_aov))
qqline(residuals(res_aov))
shapiro.test(sample(residuals(res_aov), 5000))
