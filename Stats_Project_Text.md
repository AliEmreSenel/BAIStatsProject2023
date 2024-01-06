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

