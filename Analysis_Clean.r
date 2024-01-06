data_complete = read.table("application_data_remove_na.csv", header=TRUE, sep=",")
attach(data_complete)

# Initial Dataset
to_keep = c("CODE_GENDER", "FLAG_OWN_REALTY", "CNT_CHILDREN", "AMT_INCOME_TOTAL", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "REGION_POPULATION_RELATIVE", "DAYS_BIRTH", "DAYS_EMPLOYED", "OWN_CAR_AGE", "OCCUPATION_TYPE", "CNT_FAM_MEMBERS", "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY", "ORGANIZATION_TYPE", "EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3", "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BEGINEXPLUATATION_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ELEVATORS_AVG", "ENTRANCES_AVG", "FLOORSMAX_AVG", "FLOORSMIN_AVG", "LANDAREA_AVG", "LIVINGAPARTMENTS_AVG", "NONLIVINGAPARTMENTS_AVG", "NONLIVINGAREA_AVG", "APARTMENTS_MODE", "BASEMENTAREA_MODE", "YEARS_BEGINEXPLUATATION_MODE", "YEARS_BUILD_MODE", "COMMONAREA_MODE", "ELEVATORS_MODE", "ENTRANCES_MODE", "FLOORSMAX_MODE", "FLOORSMIN_MODE", "LANDAREA_MODE", "LIVINGAPARTMENTS_MODE", "LIVINGAREA_MODE", "NONLIVINGAPARTMENTS_MODE", "NONLIVINGAREA_MODE", "APARTMENTS_MEDI", "BASEMENTAREA_MEDI", "YEARS_BEGINEXPLUATATION_MEDI", "YEARS_BUILD_MEDI", "COMMONAREA_MEDI", "ELEVATORS_MEDI", "ENTRANCES_MEDI", "FLOORSMAX_MEDI", "FLOORSMIN_MEDI", "LANDAREA_MEDI", "LIVINGAPARTMENTS_MEDI", "LIVINGAREA_MEDI", "NONLIVINGAPARTMENTS_MEDI", "NONLIVINGAREA_MEDI", "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "TOTALAREA_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE", "OBS_30_CNT_SOCIAL_CIRCLE", "DEF_30_CNT_SOCIAL_CIRCLE", "OBS_60_CNT_SOCIAL_CIRCLE", "DEF_60_CNT_SOCIAL_CIRCLE")
categorical_features = c("CODE_GENDER", "FLAG_OWN_REALTY", "CNT_CHILDREN", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "CNT_FAM_MEMBERS", "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY", "ORGANIZATION_TYPE", "OCCUPATION_TYPE", "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE")
dataset = data_complete[, to_keep]
dataset$YEARS_BIRTH <- -dataset$DAYS_BIRTH/365
dataset$DAYS_BIRTH <- NULL
dataset$YEARS_EMPLOYED <- -dataset$DAYS_EMPLOYED/365
dataset$DAYS_EMPLOYED <- NULL
for (feature_name in categorical_features) {
  dataset[[feature_name]] <- factor(dataset[[feature_name]])
}


# Transform dataset:
transformations <- function(dataset) {
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
    transformed_data <- lapply(names(dataset), function(col) {
    col_data <- dataset[[col]]
    if (col != "AMT_INCOME_TOTAL" && !is.factor(col_data)) {
        lapply(funcs, function(f) f(col_data))
    } else {
        list(col_data)  # Skip transformations for AMT_INCOME_TOTAL or if it's a factor, return as a list
    }
    })

    # Naming the new columns and creating a new dataframe
    transformed_data_df <- as.data.frame(transformed_data)
    names(transformed_data_df) <- unlist(lapply(names(dataset), function(col) {
    col_data <- dataset[[col]]
    if (col != "AMT_INCOME_TOTAL" && !is.factor(col_data)) {
        paste(rep(col, each = length(fnames)), fnames, sep = "_")
    } else {
        col  # Skip naming transformations for AMT_INCOME_TOTAL or if it's a factor, keep its original name
    }
    }))
    return (transformed_data_df)
}
transformed_data <- transformations(dataset)


# Standardize dataset:
standardize_dataset <- function(dataset) {
  standardized_dataset <- dataset
  categorical_features = c("CODE_GENDER", "FLAG_OWN_REALTY", "CNT_CHILDREN", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "CNT_FAM_MEMBERS", "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY", "ORGANIZATION_TYPE", "OCCUPATION_TYPE", "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE")
  for (feature_name in names(dataset)) {
    if (feature_name %in% setdiff(names(dataset), union(categorical_features, "AMT_INCOME_TOTAL"))) {
      standardized_dataset[[feature_name]] <- (dataset[[feature_name]] - mean(dataset[[feature_name]]))/sd(dataset[[feature_name]])
    }
  }
  return(standardized_dataset)
}
standardized_data <- standardize_dataset(transformed_data)


# Visual exploration of the dataset:
hist(standardized_data$AMT_INCOME_TOTAL)
hist(log(standardized_data$AMT_INCOME_TOTAL), freq=FALSE, ylim=c(0, 0.85))
curve(dnorm(x, mean = mean(log(standardized_data$AMT_INCOME_TOTAL)), sd = sd(log(standardized_data$AMT_INCOME_TOTAL))), add = TRUE, col = "red")
ks.test(log(standardized_data$AMT_INCOME_TOTAL), "pnorm", mean = mean(log(standardized_data$AMT_INCOME_TOTAL)), sd = sd(log(standardized_data$AMT_INCOME_TOTAL)))
shapiro.test(sample(log(standardized_data$AMT_INCOME_TOTAL), 5000))
qqnorm(log(standardized_data$AMT_INCOME_TOTAL))
qqline(log(standardized_data$AMT_INCOME_TOTAL))


correlation_vector <- cor(standardized_data[setdiff(names(standardized_data), union("AMT_INCOME_TOTAL", categorical_features))], log(standardized_data[["AMT_INCOME_TOTAL"]]))
sorted_indices <- order(correlation_vector, decreasing = TRUE)
sorted_correlation_vector <- cor(standardized_data[setdiff(names(standardized_data), union("AMT_INCOME_TOTAL", categorical_features))][sorted_indices], log(standardized_data[["AMT_INCOME_TOTAL"]]))

plot(standardized_data$REGION_POPULATION_RELATIVE_cubed, log(standardized_data$AMT_INCOME_TOTAL))
plot(standardized_data$FLOORSMAX_AVG_exp, log(standardized_data$AMT_INCOME_TOTAL))
plot(standardized_data$FLOORSMAX_MEDI_exp, log(standardized_data$AMT_INCOME_TOTAL))


      
# Starting Linear model:
original_lm = lm(dataset$AMT_INCOME_TOTAL ~ ., data = standardized_data)
summary(original_lm) # R^2 adjusted = 0.2737
plot(dataset$AMT_INCOME_TOTAL, original_lm$residuals)

original_log_lm = lm(log(standardized_data$AMT_INCOME_TOTAL) ~ ., data = standardized_data)
summary(original_log_lm) # R^2 adjusted = 0.3337
plot(log(standardized_data$AMT_INCOME_TOTAL), original_log_lm$residuals)

# MODEL SELECTION:

# STEP UP SELECTION:
# 1. OWN CODE:
step_up <- function(dataset) {
  important_features <- c()
  iteration_number = 0
  remaining_features <- setdiff(names(dataset), "AMT_INCOME_TOTAL")
  while (TRUE) {
    #print(important_features)
    iteration_number = iteration_number + 1
    print(iteration_number)
    min_p = 1
    feature_argmin <- NULL
    for (feature in remaining_features) {
      lm = lm(log(AMT_INCOME_TOTAL) ~ ., data=dataset[union(feature, important_features)])
      if (feature %in% categorical_features) {
        temp = names(summary(lm)$coefficients[,4])
        p_value = min(summary(lm)$coefficients[,4][temp[startsWith(temp, feature)]])
      }
      else {
        p_value = summary(lm)$coefficients[,4][feature]
      }
      #print(feature)
      #print(p_value)
      if (p_value <= min_p) {
        min_p = p_value
        feature_argmin = feature
      }
    }
    print(feature_argmin)
    if (min_p <= 0.05) {
      important_features <- union(important_features, feature_argmin)
    } else {
      break
    }
    remaining_features <- setdiff(remaining_features, feature_argmin)
  }
  return(important_features)
}
step_up_model_1 <- step_up(standardized_data)

step_up_model_1 <- c("REGION_RATING_CLIENT_W_CITY",
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
                    "YEARS_EMPLOYED_inverse",
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
                    "YEARS_BUILD_MEDI_cubed")

        # Using standardized_data
step_up_model_1_lm <- lm(log(transformed_data$AMT_INCOME_TOTAL) ~ ., data=standardized_data[step_up_model_1], na.action=na.omit)
summary(step_up_model_1_lm)


# STEP DOWN SELECTION:
step_down <- function(dataset) {
  important_features <- setdiff(names(dataset), "AMT_INCOME_TOTAL")
  iteration_number = 0
  while (TRUE) {
    #print(important_features)
    iteration_number = iteration_number + 1
    print(iteration_number)
    lm = lm(log(AMT_INCOME_TOTAL) ~ ., data=dataset[important_features])
    actual_p_values <- data.frame(matrix(nrow = 1, ncol = 0))
    t <- summary(lm)$coefficients[,4]
    for (i in important_features) {
        if (i %in% categorical_features) {
            actual_p_values[[i]] <- min(t[names(t)[startsWith(names(t), i)]], na.rm = TRUE)
        }
        else {
            actual_p_values[[i]] <- summary(lm)$coefficients[,4][i]
        }
    }
    max_p = max(actual_p_values, na.rm = TRUE)
    feature_argmax = names(actual_p_values)[which.max(actual_p_values)]
    print(feature_argmax)
    if (max_p > 0.05) {
      important_features <- important_features[important_features != feature_argmax]
    } else {
      break
    }
  }
  return(important_features)
}
step_down_model_1 <- step_down(standardized_data)

step_down_model_1 <- c(
  "CODE_GENDER",
  "CNT_CHILDREN",
  "NAME_INCOME_TYPE",
  "NAME_EDUCATION_TYPE",
  "NAME_FAMILY_STATUS",
  "REGION_POPULATION_RELATIVE_exp",
  "REGION_POPULATION_RELATIVE_squared",
  "REGION_POPULATION_RELATIVE_identity",
  "REGION_POPULATION_RELATIVE_cubed",
  "REGION_POPULATION_RELATIVE_cubic_root",
  "OWN_CAR_AGE_log",
  "OWN_CAR_AGE_exp",
  "OWN_CAR_AGE_inverse",
  "OCCUPATION_TYPE",
  "REGION_RATING_CLIENT",
  "REGION_RATING_CLIENT_W_CITY",
  "ORGANIZATION_TYPE",
  "EXT_SOURCE_1_squared",
  "EXT_SOURCE_2_log",
  "EXT_SOURCE_2_exp",
  "EXT_SOURCE_2_squared",
  "EXT_SOURCE_2_identity",
  "EXT_SOURCE_2_cubed",
  "EXT_SOURCE_2_inverse_squared",
  "EXT_SOURCE_3_identity",
  "APARTMENTS_AVG_log",
  "APARTMENTS_AVG_identity",
  "APARTMENTS_AVG_inverse",
  "BASEMENTAREA_AVG_log",
  "BASEMENTAREA_AVG_exp",
  "BASEMENTAREA_AVG_identity",
  "BASEMENTAREA_AVG_cubed",
  "BASEMENTAREA_AVG_inverse",
  "BASEMENTAREA_AVG_inverse_squared",
  "ELEVATORS_AVG_cubed",
  "LIVINGAPARTMENTS_AVG_exp",
  "LIVINGAPARTMENTS_AVG_squared",
  "LIVINGAPARTMENTS_AVG_identity",
  "LIVINGAPARTMENTS_AVG_cubed",
  "LIVINGAPARTMENTS_AVG_inverse",
  "LIVINGAPARTMENTS_AVG_inverse_squared",
  "NONLIVINGAPARTMENTS_AVG_log",
  "NONLIVINGAPARTMENTS_AVG_exp",
  "NONLIVINGAPARTMENTS_AVG_identity",
  "NONLIVINGAPARTMENTS_AVG_cubed",
  "NONLIVINGAPARTMENTS_AVG_inverse",
  "YEARS_BEGINEXPLUATATION_MODE_log",
  "YEARS_BEGINEXPLUATATION_MODE_exp",
  "YEARS_BEGINEXPLUATATION_MODE_sqrt",
  "YEARS_BEGINEXPLUATATION_MODE_inverse_squared",
  "YEARS_BUILD_MODE_sqrt",
  "YEARS_BUILD_MODE_squared",
  "YEARS_BUILD_MODE_identity",
  "YEARS_BUILD_MODE_cubic_root",
  "ELEVATORS_MODE_squared",
  "ENTRANCES_MODE_log",
  "ENTRANCES_MODE_sqrt",
  "ENTRANCES_MODE_identity",
  "ENTRANCES_MODE_inverse",
  "ENTRANCES_MODE_cubic_root",
  "FLOORSMAX_MODE_squared",
  "LIVINGAREA_MODE_identity",
  "YEARS_BUILD_MEDI_log",
  "YEARS_BUILD_MEDI_squared",
  "YEARS_BUILD_MEDI_identity",
  "FLOORSMIN_MEDI_cubed",
  "LIVINGAPARTMENTS_MEDI_log",
  "LIVINGAPARTMENTS_MEDI_exp",
  "LIVINGAPARTMENTS_MEDI_squared",
  "LIVINGAPARTMENTS_MEDI_cubed",
  "LIVINGAPARTMENTS_MEDI_inverse",
  "LIVINGAPARTMENTS_MEDI_inverse_squared",
  "NONLIVINGAPARTMENTS_MEDI_log",
  "NONLIVINGAPARTMENTS_MEDI_exp",
  "NONLIVINGAPARTMENTS_MEDI_identity",
  "NONLIVINGAPARTMENTS_MEDI_cubed",
  "NONLIVINGAPARTMENTS_MEDI_inverse",
  "NONLIVINGAREA_MEDI_log",
  "NONLIVINGAREA_MEDI_exp",
  "NONLIVINGAREA_MEDI_identity",
  "NONLIVINGAREA_MEDI_cubed",
  "NONLIVINGAREA_MEDI_inverse",
  "TOTALAREA_MODE_log",
  "TOTALAREA_MODE_exp",
  "TOTALAREA_MODE_squared",
  "TOTALAREA_MODE_identity",
  "TOTALAREA_MODE_cubed",
  "TOTALAREA_MODE_inverse",
  "YEARS_BIRTH_log",
  "YEARS_BIRTH_exp",
  "YEARS_BIRTH_sqrt",
  "YEARS_BIRTH_squared",
  "YEARS_BIRTH_identity",
  "YEARS_BIRTH_cubic_root",
  "YEARS_EMPLOYED_sqrt",
  "YEARS_EMPLOYED_identity"
)

        # Using transformed_data
step_down_model_1_lm_1 <- lm(log(transformed_data$AMT_INCOME_TOTAL) ~ ., data=transformed_data[step_down_model_1])
summary(step_down_model_1_lm_1)
plot(log(transformed_data$AMT_INCOME_TOTAL), step_down_model_1_lm_1$residuals)

        # Using standardized_data
step_down_model_1_lm_2 <- lm(log(transformed_data$AMT_INCOME_TOTAL) ~ ., data=standardized_data[step_down_model_1])
summary(step_down_model_1_lm_2)


# 2. USING STEPAIC:
library(MASS)

full.model <- lm(formula = log(transformed_data$AMT_INCOME_TOTAL) ~ CODE_GENDER +
    NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS +
    REGION_POPULATION_RELATIVE_exp + REGION_POPULATION_RELATIVE_squared +
    REGION_POPULATION_RELATIVE_identity + REGION_POPULATION_RELATIVE_cubed +
    REGION_POPULATION_RELATIVE_cubic_root + OWN_CAR_AGE_log +
    OWN_CAR_AGE_exp + OWN_CAR_AGE_inverse + OCCUPATION_TYPE +
    REGION_RATING_CLIENT_W_CITY + EXT_SOURCE_1_squared + EXT_SOURCE_3_identity +
    LIVINGAPARTMENTS_AVG_exp + LIVINGAPARTMENTS_AVG_squared +
    LIVINGAPARTMENTS_AVG_identity + LIVINGAPARTMENTS_AVG_cubed +
    LIVINGAPARTMENTS_AVG_inverse + LIVINGAPARTMENTS_AVG_inverse_squared +
    YEARS_BEGINEXPLUATATION_MODE_log + YEARS_BEGINEXPLUATATION_MODE_exp +
    YEARS_BEGINEXPLUATATION_MODE_sqrt + YEARS_BEGINEXPLUATATION_MODE_inverse_squared +
    YEARS_BUILD_MODE_sqrt + YEARS_BUILD_MODE_squared + YEARS_BUILD_MODE_identity +
    YEARS_BUILD_MODE_cubic_root + FLOORSMAX_MODE_squared + YEARS_BUILD_MEDI_log +
    YEARS_BUILD_MEDI_squared + YEARS_BUILD_MEDI_identity + FLOORSMIN_MEDI_cubed +
    LIVINGAPARTMENTS_MEDI_log + LIVINGAPARTMENTS_MEDI_exp + LIVINGAPARTMENTS_MEDI_squared +
    LIVINGAPARTMENTS_MEDI_cubed + LIVINGAPARTMENTS_MEDI_inverse +
    LIVINGAPARTMENTS_MEDI_inverse_squared + TOTALAREA_MODE_log + 
    TOTALAREA_MODE_exp + TOTALAREA_MODE_squared + TOTALAREA_MODE_identity +
    TOTALAREA_MODE_cubed + TOTALAREA_MODE_inverse + YEARS_BIRTH_log +
    YEARS_BIRTH_cubic_root + YEARS_EMPLOYED_sqrt + YEARS_EMPLOYED_identity,
    data = standardized_data[step_down_model_1])
step.model <- stepAIC(full.model, direction = c("backward"), trace = TRUE, k = log(nrow(standardized_data))^2)
length(summary(step.model)$coefficients)
summary(step.model)

# FIRST RESULT USING BIC
#lm(formula = log(transformed_data$AMT_INCOME_TOTAL) ~ CODE_GENDER +
#    NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS +
#    REGION_POPULATION_RELATIVE_exp + REGION_POPULATION_RELATIVE_squared +
#    REGION_POPULATION_RELATIVE_identity + REGION_POPULATION_RELATIVE_cubed +
#    REGION_POPULATION_RELATIVE_cubic_root + OWN_CAR_AGE_log +
#    OWN_CAR_AGE_exp + OWN_CAR_AGE_inverse + OCCUPATION_TYPE +
#    REGION_RATING_CLIENT_W_CITY + EXT_SOURCE_1_squared + EXT_SOURCE_3_identity +
#    LIVINGAPARTMENTS_AVG_exp + LIVINGAPARTMENTS_AVG_squared +
#    LIVINGAPARTMENTS_AVG_identity + LIVINGAPARTMENTS_AVG_cubed +
#    LIVINGAPARTMENTS_AVG_inverse + LIVINGAPARTMENTS_AVG_inverse_squared +
#    YEARS_BEGINEXPLUATATION_MODE_log + YEARS_BEGINEXPLUATATION_MODE_exp +
#    YEARS_BEGINEXPLUATATION_MODE_sqrt + YEARS_BEGINEXPLUATATION_MODE_inverse_squared +
#    YEARS_BUILD_MODE_sqrt + YEARS_BUILD_MODE_squared + YEARS_BUILD_MODE_identity +
#    YEARS_BUILD_MODE_cubic_root + FLOORSMAX_MODE_squared + YEARS_BUILD_MEDI_log +
#    YEARS_BUILD_MEDI_squared + YEARS_BUILD_MEDI_identity + FLOORSMIN_MEDI_cubed +
#    LIVINGAPARTMENTS_MEDI_log + LIVINGAPARTMENTS_MEDI_exp + LIVINGAPARTMENTS_MEDI_squared +
#    LIVINGAPARTMENTS_MEDI_cubed + LIVINGAPARTMENTS_MEDI_inverse +
#    LIVINGAPARTMENTS_MEDI_inverse_squared + TOTALAREA_MODE_log + 
#    TOTALAREA_MODE_exp + TOTALAREA_MODE_squared + TOTALAREA_MODE_identity +
#    TOTALAREA_MODE_cubed + TOTALAREA_MODE_inverse + YEARS_BIRTH_log +
#    YEARS_BIRTH_cubic_root + YEARS_EMPLOYED_sqrt + YEARS_EMPLOYED_identity,
#    data = standardized_data[step_down_model_1])

# SECOND RESULT USING BIC (WITH THE PENALTY SQUARED):
#lm(formula = log(transformed_data$AMT_INCOME_TOTAL) ~ CODE_GENDER +
#    NAME_INCOME_TYPE + NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS +
#    REGION_POPULATION_RELATIVE_exp + REGION_POPULATION_RELATIVE_squared +
#    REGION_POPULATION_RELATIVE_identity + REGION_POPULATION_RELATIVE_cubed +
#    REGION_POPULATION_RELATIVE_cubic_root + OWN_CAR_AGE_log +
#    OWN_CAR_AGE_exp + OWN_CAR_AGE_inverse + OCCUPATION_TYPE +
#    REGION_RATING_CLIENT_W_CITY + EXT_SOURCE_1_squared + EXT_SOURCE_3_identity +
#    LIVINGAPARTMENTS_AVG_exp + LIVINGAPARTMENTS_AVG_squared +
#    LIVINGAPARTMENTS_AVG_identity + LIVINGAPARTMENTS_AVG_cubed +
#    LIVINGAPARTMENTS_AVG_inverse + LIVINGAPARTMENTS_AVG_inverse_squared +
#    YEARS_BEGINEXPLUATATION_MODE_log + YEARS_BEGINEXPLUATATION_MODE_exp +
#    YEARS_BEGINEXPLUATATION_MODE_sqrt + YEARS_BEGINEXPLUATATION_MODE_inverse_squared +
#    YEARS_BUILD_MODE_sqrt + YEARS_BUILD_MODE_squared + YEARS_BUILD_MODE_identity +
#    YEARS_BUILD_MODE_cubic_root + FLOORSMAX_MODE_squared + YEARS_BUILD_MEDI_log +
#    YEARS_BUILD_MEDI_squared + YEARS_BUILD_MEDI_identity + FLOORSMIN_MEDI_cubed +
#    LIVINGAPARTMENTS_MEDI_log + LIVINGAPARTMENTS_MEDI_exp + LIVINGAPARTMENTS_MEDI_squared +
#    LIVINGAPARTMENTS_MEDI_cubed + LIVINGAPARTMENTS_MEDI_inverse +
#    LIVINGAPARTMENTS_MEDI_inverse_squared + TOTALAREA_MODE_log + 
#    TOTALAREA_MODE_exp + TOTALAREA_MODE_squared + TOTALAREA_MODE_identity +
#    TOTALAREA_MODE_cubed + TOTALAREA_MODE_inverse + YEARS_BIRTH_log +
#    YEARS_BIRTH_cubic_root + YEARS_EMPLOYED_sqrt + YEARS_EMPLOYED_identity,
#    data = standardized_data[step_down_model_1])


# DIVISION OF DATASET INTO 4 INCOME LEVELS:
cut_offs <- quantile(log(standardized_data$AMT_INCOME_TOTAL), prob=c(.25,.5,.75), type=1)
standardized_data$GROUPING <- cut(log(transformed_data$AMT_INCOME_TOTAL), breaks=c(-Inf, cut_offs, Inf), labels=c("1", "2", "3", "4"), include.lowest=TRUE)

for (i in 1:4) {
  test <- standardized_data[standardized_data$GROUPING == i,]
  test$GROUPING <- NULL
  lm = lm(log(test$AMT_INCOME_TOTAL) ~ ., data = test)
  cat(sprintf("For group: %i, R^2: %.2f, R^2 adjusted: %.2f", i, summary(lm)$r.squared, summary(lm)$adj.r.squared))
  print("\n")
}

test <- standardized_data[standardized_data$GROUPING == 3,]
test$GROUPING <- NULL
lm = lm(log(test$AMT_INCOME_TOTAL) ~ ., data = test)
summary(lm)


# HYPOTHESIS TESTING, DOES GENDER AND JOB TYPE AFFECT TOTAL INCOME? (ANOVA TIME)
res_aov <- aov(AMT_INCOME_TOTAL ~ CODE_GENDER + OCCUPATION_TYPE + CODE_GENDER:OCCUPATION_TYPE, data = standardized_data)
summary(res_aov)

# MODEL TRAINING & CROSS VALIDATION
library(modelr)
library(purrr)
library(tidyverse)
library(plyr)


get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

calc_MSE <- function(pred, target){
  MSE  <- pred %>% group_by(Run) %>%
    summarise(MSE = mean( (target - pred)^2))
  return(MSE / mean(target))
}

calc_standardized_MSE <- function(pred, target){
  return(calc_MSE(pred, target) / mean(target))
}

for (k in 2:10) {
  print(k)
  
  cv_step_down  <- crossv_kfold(transformed_data[step_down_model_1], k = k)
  cv_step_up <- crossv_kfold(transformed_data[step_up_model_1], k = k)
  cv_all  <- crossv_kfold(transformed_data, k = k)
  
  cv_standardized_step_down  <- crossv_kfold(standardized_data[step_down_model_1], k = k)
  cv_standardized_step_up  <- crossv_kfold(standardized_data[step_up_model_1], k = k)
  cv_standardized_all  <- crossv_kfold(standardized_data, k = k)

  
#  model_step_down <- map(cv_step_down$train, ~lm(AMT_INCOME_TOTAL ~ ., data=transformed_data[step_down_model_1]))
#  model_step_up <- map(cv_step_up$train, ~lm(AMT_INCOME_TOTAL ~ ., data=transformed_data[step_up_model_1]))
#  model_all <- map(cv_all$train, ~lm(AMT_INCOME_TOTAL ~ ., data=transformed_data))
  model_step_down_log <- map(cv_step_down$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=transformed_data[step_down_model_1]))
  model_step_up_log <- map(cv_step_up$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=transformed_data[step_up_model_1]))
  model_all_log <- map(cv_all$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=transformed_data))
  
#  model_standardized_step_down <- map(cv_standardized_step_down$train, ~lm(AMT_INCOME_TOTAL ~ ., data=standardized_data[step_down_model_1]))
#  model_standardized_step_up <- map(cv_standardized_step_up$train, ~lm(AMT_INCOME_TOTAL ~ ., data=standardized_data[step_up_model_1]))
#  model_standardized_all <- map(cv_standardized_all$train, ~lm(AMT_INCOME_TOTAL ~ ., data=standardized_data))
  model_standardized_step_down_log <- map(cv_standardized_step_down$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=standardized_data[step_down_model_1]))
  model_standardized_step_up_log <- map(cv_standardized_step_up$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=standardized_data[step_up_model_1]))
  model_standardized_all_log <- map(cv_standardized_all$train, ~lm(log(AMT_INCOME_TOTAL) ~ ., data=standardized_data))
  
  
#  pred_step_down  <- map2_df(model_step_down, cv_step_down$test, get_pred, .id = "Run")
#  pred_step_up  <- map2_df(model_step_up, cv_step_up$test, get_pred, .id = "Run")
#  pred_all  <- map2_df(model_all, cv_all$test, get_pred, .id = "Run")
  pred_step_down_log  <- map2_df(model_step_down_log, cv_step_down$test, get_pred, .id = "Run")
  pred_step_up_log  <- map2_df(model_step_up_log, cv_step_up$test, get_pred, .id = "Run")
  pred_all_log  <- map2_df(model_all_log, cv_all$test, get_pred, .id = "Run")
  
#  pred_standardized_step_down  <- map2_df(model_standardized_step_down, cv_standardized_step_down$test, get_pred, .id = "Run")
#  pred_standardized_step_up  <- map2_df(model_standardized_step_up, cv_standardized_step_up$test, get_pred, .id = "Run")
#  pred_standardized_all  <- map2_df(model_standardized_all, cv_standardized_all$test, get_pred, .id = "Run")
  pred_standardized_step_down_log  <- map2_df(model_standardized_step_down_log, cv_standardized_step_down$test, get_pred, .id = "Run")
  pred_standardized_step_up_log  <- map2_df(model_standardized_step_up_log, cv_standardized_step_up$test, get_pred, .id = "Run")
  pred_standardized_all_log  <- map2_df(model_standardized_all_log, cv_standardized_all$test, get_pred, .id = "Run")
  
  
#  print(calc_MSE(pred_step_down, AMT_INCOME_TOTAL))
#  print(calc_standardized_MSE(pred_step_down, AMT_INCOME_TOTAL))
#  print(calc_MSE(pred_step_up, AMT_INCOME_TOTAL))
#  print(calc_standardized_MSE(pred_step_up, AMT_INCOME_TOTAL))
#  print(calc_MSE(pred_all, AMT_INCOME_TOTAL))
#  print(calc_standardized_MSE(pred_all, AMT_INCOME_TOTAL))
#  print(calc_MSE(pred_step_down_log, log(AMT_INCOME_TOTAL)))
  print(calc_standardized_MSE(pred_step_down_log, log(AMT_INCOME_TOTAL)))
#  print(calc_MSE(pred_step_up_log, log(AMT_INCOME_TOTAL)))
  print(calc_standardized_MSE(pred_step_up_log, log(AMT_INCOME_TOTAL)))
#  print(calc_MSE(pred_all_log, log(AMT_INCOME_TOTAL)))
  print(calc_standardized_MSE(pred_all_log, log(AMT_INCOME_TOTAL)))
  
#  print(calc_MSE(pred_standardized_step_down, AMT_INCOME_TOTAL))
#  print(calc_standardized_MSE(pred_standardized_step_down, AMT_INCOME_TOTAL))
#  print(calc_MSE(pred_standardized_step_up, AMT_INCOME_TOTAL))
#  print(calc_standardized_MSE(pred_standardized_step_up, AMT_INCOME_TOTAL))
#  print(calc_MSE(pred_standardized_all, AMT_INCOME_TOTAL))
#  print(calc_standardized_MSE(pred_standardized_all, AMT_INCOME_TOTAL))
#  print(calc_MSE(pred_standardized_step_down_log, log(AMT_INCOME_TOTAL)))
  print(calc_standardized_MSE(pred_standardized_step_down_log, log(AMT_INCOME_TOTAL)))
#  print(calc_MSE(pred_standardized_step_up_log, log(AMT_INCOME_TOTAL)))
  print(calc_standardized_MSE(pred_standardized_step_up_log, log(AMT_INCOME_TOTAL)))
#  print(calc_MSE(pred_standardized_all_log, log(AMT_INCOME_TOTAL)))
  print(calc_standardized_MSE(pred_standardized_all_log, log(AMT_INCOME_TOTAL)))
}

# LASSO Model
library(glmnet)

x = data.matrix(standardized_data[,!names(standardized_data) %in% c("AMT_INCOME_TOTAL")])
y = log(standardized_data$AMT_INCOME_TOTAL)

cv_model <- cv.glmnet(x, y, alpha=1)

best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda )
best_coef <- coef(best_model)
best_coef

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# Calculate adjusted R^2
n <- length(y)  # Number of observations
k <- sum(best_coef != 0)  # Number of predictors with non-zero coefficients

adjusted_rsq <- 1 - ((1 - rsq) * (n - 1)) / (n - k - 1)
adjusted_rsq

