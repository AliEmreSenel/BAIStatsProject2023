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
  function(x) 1/x,
  function(x) 1/(x^2),
  function(x) x^(1/3),
)
fnames <- c("log+1", "exp", "sqrt", "x^2", "identity", "x^3", "1/x", "1/x^2", "x^(1/3)")

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
hist(log(data$AMT_INCOME_TOTAL))

# Predicting the log of the income increases the R^2
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


