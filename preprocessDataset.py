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

print(listMissingColumns(df))

print(countMissingPerRow(df))
pd.set_option('display.max_rows', None)  
print(df.dtypes)

print(df.loc[df["missing"] > 20, :].sample(10))

#columns with the most missing values
missing_values = df.isnull().sum().sort_values(ascending=False).loc[lambda x: x>0]
with pd.option_context('display.max_rows', None, 'display.max_columns', None):  # more options can be specified also
    print(missing_values)

df.to_csv('application_data_preprocessed.csv', index=False)
