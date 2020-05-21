import pandas as pd
import numpy as np
import datetime as dt
import re
import requests
from bs4 import BeautifulSoup
from tqdm import tqdm
import os


headers = {'User-Agent':
           'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36'}

subjects = []
for page_nr in range(1, 100):
    try:
        url = f'https://catalog.osaarchivum.org/catalog/facet/subject_person_facet?facet.page={page_nr}&q=hungarian+radio+monitoring&search_field=search_all&utf8=%E2%9C%93'

        tree = requests.get(url, headers=headers)
        soup = BeautifulSoup(tree.content, 'html.parser')

        spans = soup.find('ul', {'class': 'facet-values list-unstyled'}).find_all('li')

        for span in spans:
            subjects.append({'name': span.find('a').text,
                             'count': span.find('span', {'class': 'facet-count'}).text})

    except Exception as e:
        print(page_nr)
        break

subjects = pd.DataFrame(subjects)

subjects.to_csv('data/processed/subjects.csv', index=False)
