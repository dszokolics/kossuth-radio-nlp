import os
import re
import datetime as dt
import pandas as pd
from tika import parser
from tqdm import tqdm
import textdistance


### Rename pdfs

path = 'data/raw/'
files = os.listdir(path)

f = files[0]
f

'Hungarian_monitoring_'+dt.datetime.strptime(re.split(', |\.', f)[1], '%d %B %Y').strftime('%Y_%m_%d') + '.pdf'

catalog = pd.DataFrame([[path + f, dt.datetime.strptime(re.split(', |\.', f)[1], '%d %B %Y')] for f in files])
catalog.columns = ['file_name', 'date']

catalog.sort_values('date')

catalog.to_csv('data/processed.catalog.csv', index=False)

pd.date_range(start = '1988-01-01', end = '1990-12-04' ).difference(catalog.date)

date_list = []
for f in tqdm(files[:200]):
    raw = parser.from_file(path+f)
    c = raw['content']
    c = [x for x in c.split('\n')]  # if x != '']

    date_list.append([x for x in c if x != ''][:10])

months_hun = ['január', 'február', 'március', 'április', 'május', 'június',
              'július', 'augusztus', 'szeptember', 'október', 'november', 'december']

dl = [''.join(x)[''.join(x).find('R I N G')+8:].replace(' ', '') for x in date_list]
parsed_header = []
for l in dl:
    min_match = 999
    for month in months_hun:
        month_nr = l.lower().find(month)
        if (month_nr >= 0) & (month_nr < min_match):
            min_match = month_nr
            current_month = month
    res = {'year': l[:min_match],
           'month': current_month,
           'day': l[min_match+len(current_month):]}
    parsed_header.append(res)

df = pd.DataFrame(parsed_header)


def clean_year(year):
    try:
        match = re.search('[0-9]{4}', year).group(0)
        if match in ['1988', '1989', '1990']:
            return match
    except Exception as e:
        if re.search('199o', year) is not None:
            return '1990'
        else:
            return None


def clean_day(day):
    try:
        match = re.match('[0-9]{1,2}', day).group(0)
        if 0 < int(match) < 32:
            return match
    except Exception as e:
        return None


def create_date(d):
    try:
        # print(dt.date(int(d['year_c']), int(d['month_c']), int(d['day_c'])))
        return dt.date(int(d['year_c']), int(d['month_c']), int(d['day_c']))
    except Exception as e:
        print(e)
        return None


df['year_c'] = df.year.apply(clean_year).astype(float)
df['day_c'] = df.day.apply(clean_day).astype(float)
df['month_c'] = df.month.apply(lambda x: months_hun.index(x)+1)
df['date_c'] = df.apply(lambda x: create_date(x), axis=1)

re.search('[0-9]{4}', 'HUNGÁRIÁNMONITORINX990')

re.match('[0-9]{1,2}', '1Nagypéntek').group(0)

df.iloc[130].year[1:].lower().find('május')

dt.date(int(df.iloc[0]['year_c']), int(df.iloc[0]['month_c']), int(df.iloc[0]['day_c']))

data = pd.concat([catalog.iloc[:200], df], axis=1)

data[data.date != data.date_c]
data.head()

df.head()

sum(df.date.isna())

sum(~df.day_c.isna())
df[df.day_c.isna()]

sum(df.year_c.isin([1988, 1989, 1990]))
df[~df.year_c.isin([1988, 1989, 1990])]

df.year_c

parsed_header
