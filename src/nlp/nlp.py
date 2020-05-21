import pandas as pd


df = pd.read_csv('data/processed/almost_tidy_text.csv')

df.shape

df.head(50)

df.text.values[16]
