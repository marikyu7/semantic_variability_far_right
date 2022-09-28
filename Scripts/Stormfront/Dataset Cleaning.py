import pandas as pd
import nltk
words = set(nltk.corpus.words.words())
import cleantext
from math import nan


dfSf = pd.read_csv(r'.data/Raw_Data/stormfront_data_full.csv')
dfSf.drop('Unnamed: 0', inplace=True, axis=1)
dfSf_clean = dfSf.copy()

# remove non-english words, extras spaces, stopwords, numbers, punctuation. Lower-case, stem and tokenize posts
dfSf_clean = dfSf_clean[dfSf_clean['stormfront_lang_id'] == 19]
for n in range(0,len(dfSf_clean)):
    dfSf_clean.iat[n, 0] = " ".join(w for w in nltk.wordpunct_tokenize(str(dfSf_clean.iat[n, 0])) \
                                    if w.lower() in words or not w.isalpha())
    if dfSf_clean.iat[n, 0] == '':
        dfSf_clean.iat[n, 0] = nan
    dfSf_clean.iat[n, 0] = cleantext.clean(str(dfSf_clean.iat[n,0]))
    dfSf_clean.iat[n, 0] = dfSf_clean.iat[n, 0].replace('quot', '')
    #dfSf_clean.iat[n, 0] = dfSf_clean.iat[n, 0].replace('de', '')
    #dfSf_clean.iat[n, 0] = dfSf_clean.iat[n, 0].replace('la', '')
    #dfSf_clean.iat[n, 0] = dfSf_clean.iat[n, 0].replace('en', '')
    #dfSf_clean.iat[n, 0] = dfSf_clean.iat[n, 0].replace('un', '')

# remove comments with less than 2 words
for n in range(0, len(dfSf_clean)):
    dfSf_clean.iat[n,0] = ' '.join([w for w in dfSf_clean.iat[n,0].split() if len(w) > 1])
    if len(dfSf_clean.iat[n,0].split()) < 2:
        dfSf_clean.iat[n,0] = nan
dfSf_clean = dfSf_clean.dropna(subset=['stormfront_self_content'])

dfSf_clean.to_csv('.data/dfSf.csv', index=False)

del(n, nan, words)

