import os
import configparser
import eikon as ek
import pandas as pd

os.chdir('Z:/Desktop')
config = configparser.ConfigParser()
config.read('eikon.cfg')
ek.set_app_key(config['eikon']['app_id'])

ticker_df = pd.read_csv('sp500_constituents_cleaned.csv')

cusips = ticker_df['co_cusip'].dropna().unique().tolist()

tick_df = ek.get_symbology(cusips, from_symbol_type='CUSIP',to_symbol_type='RIC', bestMatch=False)
tick_df.drop('error', axis=1, inplace=True)
df = pd.concat([tick_df, tick_df['bestMatch'].apply(pd.Series)], axis=1)
df = df.drop(['bestMatch', 'error'], axis=1).reset_index(drop=True)
df.rename(columns={'symbol':'co_cusip'}, inplace=True)

df['best_match'] = df['RICs'].map(lambda x: x[0] if isinstance(x, list) else x)
df.drop(['RIC'], axis=1, inplace=True)
df.rename(columns={'best_match': 'RIC'}, inplace=True)

ticker_df = ticker_df.merge(df, how='outer')

ticker_df.to_csv('sp500_constituents_rics.csv', index=False)