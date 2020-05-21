import os
from tika import parser
import pandas as pd
import numpy as np
from tqdm import tqdm
import re

from bs4 import BeautifulSoup


"""Clean imput data and turn it into tidy text format for NLP in R."""

path = 'data/raw/'
files = os.listdir(path)

data = {}
for f in tqdm(files):
    raw = parser.from_file(path+f, xmlContent=True)
    data[f] = raw['content']


def comprehensible(text):
    return (('onitoring' in text)
            | ('ONITORING' in text)
            | ('Hung' in text)
            | ('hung' in text)
            | ('és' in text))


def filter(div): return any([comprehensible(p.text) for p in div.find_all('p')])
def flatten(ls): return [item for sublist in ls for item in sublist]

def headline(text):
    return ((re.search('[0-9A-Z]+\s[0-9.]+\s{0,}h$', text.strip()) is not None)
             | (np.sum([s.isupper() | s.isnumeric() for s in text]) / len(text) > 0.5))


dfs = []
garbage = {}
for k, d in data.items():
    rrr = [[[n+'\n' if headline(n) else n \
             for n in re.split(r'\s*\n\s*', re.sub(r'\s*\u00ad\n\s*', '', p.text).strip())] \
             for p in div.find_all('p') if len(p.text) > 3] \
             for div in BeautifulSoup(d).find_all('div') if filter(div)]
    rrr = flatten([' '.join(x).split('\n') for x in flatten(rrr)])
    rrr = [x for x in rrr if (len(x) > 5) & (not (('onitoring' in x) | ('ONITORING' in x)) & (len(x) < 35))]
    df = pd.DataFrame(rrr)
    df['document'] = k
    df.reset_index(inplace=True)
    df.rename(columns={'index': 'paragraph', 0: 'text'}, inplace=True)
    dfs.append(df)

    garbage[k] = np.sum([not filter(div) for div in BeautifulSoup(d).find_all('div')])


pd.DataFrame(garbage, index=[0]).transpose().hist()

almost_tidy_text = pd.concat(dfs)[['document', 'paragraph', 'text']]

almost_tidy_text.shape
almost_tidy_text.tail()

catalog = pd.read_csv('data/processed/catalog.csv')
catalog['document'] = catalog.file_name.apply(lambda x: x.split('/')[-1].strip())
catalog.head()

almost_tidy_text  = pd.merge(almost_tidy_text, catalog[['document', 'date']], how='inner', on='document')

df = almost_tidy_text.copy()

df.shape

# df = pd.read_csv('data/processed/almost_tidy_text_200.csv')

re.search('[0-9.]+\s{0,}h$', 'HÍREK, TUDÓSÍTÁSOK 7.00 h'.strip()).group(0)

df.head(50)

def filter_1(x): return re.search('[0-9A-Z]+\s[0-9.]+\s{0,}h$', x.strip()) is not None
def filter_2(x): return np.sum([s.isupper() | s.isnumeric() for s in x]) / len(x) > 0.5
df['headline'] = df.text.apply(lambda x: filter_1(x) | filter_2(x))
df.groupby('date').headline.sum()

almost_tidy_text.to_csv('data/processed/almost_tidy_text_cl.csv', index=False)
