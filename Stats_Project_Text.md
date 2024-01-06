Research Project – Mathematical Statistics
Calda Lorenzo, Senel Ali Emre
January 2024

# Introduction:
Accurately assessing a person's earning capabilities is invaluable in the financial landscape, benefiting private financial institutions by enhancing client profiling and aiding revenue agencies in 
identifying discrepancies between declared and actual earnings. In this paper we seek to investigate the relationship between the total monthly income and the financial and demographic data of a person; 
with the final aim of finding key factors which can be used to model the total income of an individual. Acknowledging the complexity of this analysis, we recognize that our study may not fully encapsulate 
all determinants, but we hope our statistical analysis will provide useful insights.

# Dataset:
Given the sensitive subject of our analysis, finding a suitable dataset proved to be challenging. Ultimately, we chose to utilize the Home Credit Group Kaggle competition dataset on loan default prediction 
and adapted it to our needs **[insert links to both competition and where we got the data]**. We recognize that using a dataset originally intended for a different purpose may introduce biases, and we will 
address this concern in subsequent discussions. Originally, the dataset contained over 122 columns providing information about customers and their loan repayment abilities. Our modifications, detailed below, 
resulted in a dataset comprising 8602 data points, 69 features, and the target variable AMT_TOTAL_INCOME. A description of the features is available at “columns_description.csv” which has been included 
along with the dataset.

### Data cleaning:
In our initial dataset modification, we specifically targeted the removal of columns related solely to the loan application process, deeming them unnecessary for our analysis. We also engaged in further data 
cleaning operations by removing data points with missing columns resulting in a reduction of our total data points from 307,511 to 8,602. We also transformed the variables changing DAYS_BIRTH and 
DAYS_EMPLOYED into YEARS_BIRTH and YEARS_EMPLOYED by dividing both columns by 365.

### Data transformation and normalization:
The relationship between the features and the target variable is not necessarily linear in nature. To address this potential nonlinear relationships between features and the target variable, we developed a 
function called transformations() that, for each continuous feature in the dataset, generates various transformations, such as logarithmic, square root, square, cube, exponential, inverse, inverse squared, 
and cube root. The logarithm, the inverse and the inverse squared were adjusted to be able to satisfy the domain requirements of some of the features, for example the logarithm results in the transformation 
log(x+1). These transformations expanded the feature set to 493, encompassing multiple orders of magnitude. However, this diversity might impact our plan to perform feature selection using LASSO penalization, as features would be weighted differently. To mitigate this, we standardized the data by subtracting the mean from each column and dividing by the standard deviation. This ensures a consistent scale across features, addressing potential issues in the LASSO penalty application.

# Preliminary Data Exploration:
We initiated our analysis with a visual exploration of the data. Utilizing the hist() function, we created a histogram for our target variable, AMT_INCOME_TOTAL. The histogram revealed a pronounced right skew, with a majority of data concentrated in the first income bin. This observation aligns with our expectation that incomes follow approximately a log-normal distribution. Plotting the logarithm of AMT_INCOME_TOTAL yields a more manageable empirical distribution resembling a normal distribution. Both a Kolmogorov-Smirnov test and a Shapiro-Wilkins test of normality reject the hypothesis that log(AMT_INCOME_TOTAL) is normally distributed, however these results are to be taken with a grain of salt since the large sample size makes it simpler for these methods to find evidence against the null hypothesis. Consequently, we made the decision to transform our target variable from AMT_INCOME_TOTAL to log(AMT_INCOME_TOTAL). This transformation aims to mitigate the influence of high earners on the linear model by reducing skewness. Any potential issues arising from this decision will be addressed later in the analysis. **[Insert plots of AMT_INCOME_DATA and its log]**.
Shifting our focus to the covariates, our initial examination involved assessing the correlation between our target variable (the logarithm of income) and the non-categorical covariates. The correlation coefficients range between 0.27 and -0.27, with the most correlated variable being REGION_POPULATION_RELATIVE_cubed, which is the cubed transformation of a measure of population in the region of residence of the person. Turning to the categorical covariates, we first analyzed the relative frequency of each level in the categorical variable and then generated boxplots to visualize the relationships between these categorical covariates and the target variable, focusing on those we hypothesized to be better predictors of income. **[Insert box plots]**.

# Linear Regression:
In the upcoming section, our focus shifts to multivariate linear regression, aiming to construct a predictive model for our target variable using a subset of dataset features. We plan to employ three distinct model selection methods, evaluating their performance through k-fold cross-validation. This technique involves dividing the dataset into k parts, using each as a validation set, and the union of the remaining k-1 parts as the training dataset. This allows us to reduce the risk of overfitting, a particularly pertinent concern given the abundance of features in our dataset. 

### Step-up model:
The first feature selection model we employ is the step-up/forward selection model, a form of stepwise regression model. Beginning with a parameter-free model, it tests each parameter individually. If no null hypotheses are rejected (p-value > 0.05), the model without parameters is chosen. Otherwise, the parameter with the smallest p-value is added, and the process is iteratively repeated until a suitable model is selected. While this method of model selection is quick to implement and often works well in finding small subsets of the covariates, it is worth mentioning that it has been criticized since the final model depends on the specific data used and thus may behave poorly out-of-sample. Since it strays from the scope of this paper, we link to a more in-depth discussion of this phenomenon. [insert endnote to paper].
This method results in a linear model with 26 features, achieving an R^2 of 0.3354 and an adjusted R^2 of 0.3268. The detailed feature list is available in the accompanying R file.

### Step-down model:
The step-down/backward elimination model, akin to step-up, is a form of stepwise regression. Unlike step-up, it starts with the full model and iteratively tests each parameter. If all null hypotheses are rejected, the entire model is chosen. Otherwise, the parameter with the largest p-value is removed, and the process is repeated. Given the similarities with the step-up method it also suffers from the same drawbacks we discussed above. Importantly, results from the step-up and step-down methods may differ.
This method produces a linear model with 96 features, achieving an R^2 of 0.3495 and an adjusted R^2 of 0.3357.

### LASSO:
LASSO regression is another popular method for feature selection, it incorporates a penalty term on the absolute values of coefficients to promote sparsity. A crucial step in LASSO is selecting the optimal hyperparameter lambda, which controls the magnitude of the l_1 norm penalization. To determine the best lambda, the function cv.glmnet() is employed, automatically choosing the lambda associated with the minimum cross-validated mean squared error.
This method results in a linear model with 59 features, achieving an R^2 of 0.3323 and an adjusted R^2 of 0.3207.
