import pandas as pd

df = pd.read_csv('application_data.csv')

print(df.shape)
print(df.head())

#dropping flag_document
to_drop = [i for i in df.columns.tolist() if i.startswith('FLAG_DOCUMENT')]
df = df.drop(to_drop, axis=1)

def listMissingColumns(df):
    missingColumns = []
    for column in df.columns:
        if df[column].isnull().any():
            missingColumns.append(column)
            if df[column].dtype == 'object':
                print(column, df[column].isnull().sum(), df[column].unique())
            else:
                print(column, df[column].isnull().sum(), df[column].min(), df[column].max())
    return missingColumns

def countMissingPerRow(df):
    df['missing'] = df.isnull().sum(axis=1)
    return df['missing'].value_counts().sort_index()

def replaceMissingValues(column, value):
    df[column].fillna(value, inplace=True)

print(df.sample(10))

df["TARGET"] = df["TARGET"].map({0: "client /w payment difficulties", 1: "other"})
for column in df.columns:
    for string in ["FLAG", "REG_REGION_NOT_LIVE_REGION", "REG_REGION_NOT_WORK_REGION", "LIVE_REGION_NOT_WORK_REGION", "REG_CITY_NOT_LIVE_CITY", "REG_CITY_NOT_WORK_CITY", "LIVE_CITY_NOT_WORK_CITY"]:
        if string in column:
            df[column] = df[column].map({0: "N", 1: "Y", "N": "N", "Y": "Y"}, na_action='ignore')
            print(column, df[column].unique())
    if "REGION_RATING" in column:
        df[column] = df[column].map({1: "bad", 2: "average", 3: "good"}, na_action='ignore')
        print(column, df[column].unique())

#replaceMissingValues('AMT_ANNUITY', 0.0)
#replaceMissingValues('AMT_GOODS_PRICE', 0.0)

cols_to_keep = ["CODE_GENDER", "FLAG_OWN_REALTY", "CNT_CHILDREN", "AMT_INCOME_TOTAL", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "REGION_POPULATION_RELATIVE", "DAYS_BIRTH", "DAYS_EMPLOYED", "OWN_CAR_AGE", "OCCUPATION_TYPE", "CNT_FAM_MEMBERS", "REGION_RATING_CLIENT", "REGION_RATING_CLIENT_W_CITY", "ORGANIZATION_TYPE", "EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3", "APARTMENTS_AVG", "BASEMENTAREA_AVG", "YEARS_BEGINEXPLUATATION_AVG", "YEARS_BUILD_AVG", "COMMONAREA_AVG", "ELEVATORS_AVG", "ENTRANCES_AVG", "FLOORSMAX_AVG", "FLOORSMIN_AVG", "LANDAREA_AVG", "LIVINGAPARTMENTS_AVG", "NONLIVINGAPARTMENTS_AVG", "NONLIVINGAREA_AVG", "APARTMENTS_MODE", "BASEMENTAREA_MODE", "YEARS_BEGINEXPLUATATION_MODE", "YEARS_BUILD_MODE", "COMMONAREA_MODE", "ELEVATORS_MODE", "ENTRANCES_MODE", "FLOORSMAX_MODE", "FLOORSMIN_MODE", "LANDAREA_MODE", "LIVINGAPARTMENTS_MODE", "LIVINGAREA_MODE", "NONLIVINGAPARTMENTS_MODE", "NONLIVINGAREA_MODE", "APARTMENTS_MEDI", "BASEMENTAREA_MEDI", "YEARS_BEGINEXPLUATATION_MEDI", "YEARS_BUILD_MEDI", "COMMONAREA_MEDI", "ELEVATORS_MEDI", "ENTRANCES_MEDI", "FLOORSMAX_MEDI", "FLOORSMIN_MEDI", "LANDAREA_MEDI", "LIVINGAPARTMENTS_MEDI", "LIVINGAREA_MEDI", "NONLIVINGAPARTMENTS_MEDI", "NONLIVINGAREA_MEDI", "FONDKAPREMONT_MODE", "HOUSETYPE_MODE", "TOTALAREA_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE", "OBS_30_CNT_SOCIAL_CIRCLE", "DEF_30_CNT_SOCIAL_CIRCLE", "OBS_60_CNT_SOCIAL_CIRCLE", "DEF_60_CNT_SOCIAL_CIRCLE"]
#df = df[cols_to_keep]

print(listMissingColumns(df))

print(countMissingPerRow(df))
pd.set_option('display.max_rows', None)  
print(df.dtypes)

print(df.loc[df["missing"] > 20, :].sample(10))

#columns with the most missing values
missing_values = df.isnull().sum().sort_values(ascending=False).loc[lambda x: x>0]
with pd.option_context('display.max_rows', None, 'display.max_columns', None):  # more options can be specified also
    print(missing_values)

#Get missing colums per number of missing values
for i in [0,1,2,3]:#countMissingPerRow(df).index:
    print("=========================================")
#    i = 1
    print(i, df.loc[df["missing"] == i, :].isnull().sum().sort_values(ascending=False).loc[lambda x: x>0])

df.to_csv('application_data_preprocessed.csv', index=False)

application_data_remove_na = df.loc[df['missing'] == 0]

len(application_data_remove_na)

application_data_remove_na.to_csv('application_data_remove_na.csv', index=False)
