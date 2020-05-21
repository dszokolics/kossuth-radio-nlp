import pandas as pd


neg = pd.read_csv('data/utils/PrecoSenti/PrecoNeg.txt', header=None, names=['word'])
pos = pd.read_csv('data/utils/PrecoSenti/PrecoPos.txt', header=None, names=['word'])

neg['sentiment'] = -1
pos['sentiment'] = 1

sentiments = pd.concat([pos, neg])

sentiments.to_csv('data/utils/sentiments-hu.csv', index=False)
