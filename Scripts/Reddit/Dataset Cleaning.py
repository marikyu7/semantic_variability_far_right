import pandas as pd  # For data handling
import nltk
words = set(nltk.corpus.words.words())
import cleantext
from math import nan

dfRC = pd.read_csv(r'.data/Raw_Data/Reddit/!RC_2006-2010.csv')
dfRC.drop('Unnamed: 0', inplace=True, axis=1)
dfRC_clean = dfRC.copy()

# remove non-english words, extras spaces, stopwords, numbers, punctuation. Lower-case, stem and tokenize posts
for n in range(len(dfRC_clean)):
    dfRC_clean.iat[n, 0] = " ".join(w for w in nltk.wordpunct_tokenize(str(dfRC_clean.iat[n, 0])) \
                                    if w.lower() in words or not w.isalpha())
    if dfRC_clean.iat[n, 0] == '':
        dfRC_clean.iat[n, 0] = nan
    dfRC_clean.iat[n, 0] = cleantext.clean(str(dfRC_clean.iat[n,0]))

# remove comments with less than 2 words and 1 letter words
for n in range(len(dfRC_clean)):
    dfRC_clean.iat[n,0] = ' '.join([w for w in dfRC_clean.iat[n,0].split() if len(w) > 1])
    if len(dfRC_clean.iat[n,0].split()) < 2:
        dfRC_clean.iat[n,0] = nan

dfRC_clean = dfRC_clean.dropna(subset=['body'])

# save the work done so far
dfRC_clean.to_csv('.data/dfRC.csv', index=False)

del(n, nan, words)


