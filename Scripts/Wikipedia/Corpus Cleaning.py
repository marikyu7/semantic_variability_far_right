import pandas as pd  # For data handling
import nltk
words = set(nltk.corpus.words.words())
import cleantext
from math import nan
from time import time
from random import sample
import random
import pickle

with open('.data/Raw_Data/wiki.txt') as f:
    wiki = f.readlines()
del(f)

corpus_wiki = pd.Series(wiki)

# remove non-english words, extras spaces, stopwords, numbers, punctuation. Lower-case, stem and tokenize posts
for n in range(len(wiki)):
    corpus_wiki[n] = " ".join(w for w in nltk.wordpunct_tokenize(str(corpus_wiki[n])) \
                                    if w.lower() in words or not w.isalpha())
    if corpus_wiki[n] == '':
        corpus_wiki[n] = nan
    corpus_wiki[n] = cleantext.clean(str(corpus_wiki[n]))


# select random text for reference
t = time()
random_wiki = []
random_wiki_tokens = 0  # set tokens counter to 0
r = list(range(len(corpus_wiki)))  # set the list from where you will sample
for row in range(len(corpus_wiki)):
    if random_wiki_tokens > (500 * 15):  # I want (about) 7.500 tokens
        break  # so when the counter reach that level, just stop the process: I have the n. of tokens I want
    else:
        n = random.choice(r)  # pick a random n. from the list for random sampling
        post = corpus_wiki[n]  # select the row that correspond to n.
        post_tokens = len(post)  # count tokens in the comment of the selected row
        random_wiki_tokens += post_tokens  # update tokens counter
        random_wiki.append(post)  # add the comment to the final user's comment selection
        r.remove(n)  # remove n. from the sampling list so you don't select a post/comment twice/

print('Time to select random wiki abstract: {} mins'.format(round((time() - t) / 60, 2)))

# remove randomly selected post from the core
corpus_wiki = corpus_wiki.loc[r]

# save the work done so far
corpus_wiki.to_csv('.data/corpus_wiki.csv', index=False)

sample_list = random_wiki
file_name = '.data/random_wiki.pkl'
open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()

del(n, nan, words)


