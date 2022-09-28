### To run in terminal with the following code:
# PYTHONHASHSEED=0 python '/Users/Marilu/Desktop/NLP/@ FYP/Scripts/Sf3 - w2v Model training.py'

import pandas as pd
import pickle
from gensim.models.phrases import Phrases, Phraser
import multiprocessing
from gensim.models import Word2Vec
from time import time

w2v_core_model = Word2Vec.load('.data/Word2Vec/w2v_core_model')

open_file = open(r'.data/Sf_users_posts.pkl', "rb")
Sf_users_posts = pickle.load(open_file)
open_file.close()

del(open_file)

### Incremental training
words = ['white', 'black', 'race', 'peopl', 'nation', 'immigr'] #brown?
w2v_Sf_users_vectors = [None]*len(Sf_users_posts)
w2v_Sf_users_neighbor = [None]*len(Sf_users_posts)
t = time()
for u in range(len(Sf_users_posts)):
    sentSf_user = [row.split() for row in Sf_users_posts[u]['stormfront_self_content']]
    phrasesSf_user = Phrases(sentSf_user, progress_per=10000)
    bigramSf_user = Phraser(phrasesSf_user)
    sentencesSf_user = bigramSf_user[sentSf_user]
    w2v_model_inc = w2v_core_model
    w2v_model_inc.train(sentencesSf_user, total_examples=w2v_model_inc.corpus_count, epochs=30)
    vectors = w2v_model_inc.wv
    my_neighbor = {'white': [None], 'black': [None], 'race': [None], 'peopl': [None],
                  'nation': [None], 'immigr': [None]}
    my_vectors = {'white': [None], 'black': [None], 'race': [None], 'peopl': [None],
                  'nation': [None], 'immigr': [None]}
    for word in words:
        my_vectors[word] = vectors[word]
        user_word_neighbor = vectors.most_similar(word, topn=round(len(vectors) / 10))
        for w in range(len(user_word_neighbor)):
            user_word_neighbor[w] = user_word_neighbor[w][0]
        my_neighbor[word] = user_word_neighbor
    w2v_Sf_users_vectors[u] = my_vectors
    w2v_Sf_users_neighbor[u] = my_neighbor
    w2v_core_model = Word2Vec.load('.data/Word2Vec/w2v_core_model')

print('Time to train 500 models: {} mins'.format(round((time() - t) / 60, 2)))

sample_list = w2v_Sf_users_vectors
file_name = '.data/Word2Vec/w2v_Sf_users_vectors.pkl'

open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()

sample_list = w2v_Sf_users_neighbor
file_name = '.data/Word2Vec/w2v_Sf_users_neighbor.pkl'

open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()

del(bigramSf_user, my_neighbor, my_vectors, phrasesSf_user, sentSf_user, sentencesSf_user, t, u, user_word_neighbor,
    vectors, w, w2v_model_inc, word)
