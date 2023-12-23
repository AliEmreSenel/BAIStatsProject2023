import pandas as pd

df = pd.read_csv('application_data.csv')

print(df.shape)
print(df.head())

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

print(listMissingColumns(df))

print(countMissingPerRow(df))
pd.set_option('display.max_rows', None)  
print(df.dtypes)

print(df.loc[df["missing"] > 20, :].sample(10))

df.to_csv('application_data_preprocessed.csv', index=False)
