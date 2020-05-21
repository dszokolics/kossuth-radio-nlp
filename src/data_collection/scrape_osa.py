import pandas as pd
import numpy as np
import datetime as dt
import re
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm
import os


### Collect the pdfs from OSA's website

headers = {'User-Agent':
           'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36'}

docs = []
for page in range(1, 12):
    url = f'https://catalog.osaarchivum.org/?page={page}&per_page=100&q=hungarian+radio+monitoring&search_field=search_all'

    tree = requests.get(url, headers=headers)
    soup = BeautifulSoup(tree.content, 'html.parser')

    d = soup.find('div', {'id': 'documents', 'class': 'documents-list'}).find_all('div', {'class': 'document'})
    docs += [[x.find('div', {'class': 'title-display'}).find('a').text, 'https://catalog.osaarchivum.org'+x.find('div', {'class': 'title-display'}).find('a')['href']] for x in d]

len(docs)


def check_filename(name):
    if name+'.pdf' in os.listdir('data/raw'):
        return check_filename(name+'(1)')
    else:
        return name


for doc in tqdm(docs):
    try:
        tree = requests.get(doc[1], headers=headers)
        soup = BeautifulSoup(tree.content, 'html.parser')

        url2 = soup.find('a', {'class': 'repository-viewer'})['href']

        file_name = check_filename(doc[0])

        response = requests.get(url2, stream=True)
        with open(f'data/raw/{file_name}.pdf', 'wb') as f:
            for chunk in tqdm(response.iter_content(2000)):
                f.write(chunk)

    except Exception as e:
        print(doc[0])
        print(e)
