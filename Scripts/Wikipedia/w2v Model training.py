### To run in terminal with the following code:
# PYTHONHASHSEED=0 python '/Users/Marilu/Desktop/NLP/@ FYP/Scripts/wiki2 - w2v Model training.py'

import pandas as pd
from gensim.models.phrases import Phrases, Phraser
from gensim.models import Word2Vec
from gensim.test.utils import datapath
import pickle
from time import time

corpus_wiki = pd.read_csv(r'.data/corpus_wiki.csv')
open_file = open(r'.data/random_wiki.pkl', "rb")
random_wiki = pickle.load(open_file)
open_file.close()


# Prepare Reddit comments
sent_wiki = [row.split() for row in corpus_wiki['0']]
phrases_wiki = Phrases(sent_wiki, min_count=10, progress_per=10000)
bigram_wiki = Phraser(phrases_wiki)
sentences_wiki = bigram_wiki[sent_wiki]

del(bigram_wiki, phrases_wiki, sent_wiki)

##### Word2Vec #####
# Train the core model
w2v_core_model = Word2Vec(window=4,
                          vector_size=200,
                          workers=1,
                          sg = 0,        # CBOW training algorithm
                          hs = 0 ,      # negative sampling
                          alpha = 0.05,
                          epochs = 6   # n. of iterations
                          )

t = time()
w2v_core_model.build_vocab(sentences_wiki, progress_per=10000)
print('Time to build vocab: {} mins'.format(round((time() - t) / 60, 2)))

t = time()
w2v_core_model.train(sentences_wiki, total_examples=w2v_core_model.corpus_count, epochs=30, report_delay=1)
print('Time to train the model: {} mins'.format(round((time() - t) / 60, 2)))

w2v_core_model.save(".data/Word2Vec/w2v_core_model")

w2v_core_vectors = w2v_core_model.wv
sample_list = w2v_core_vectors
file_name = '.data/Word2Vec/w2v_core_vectors.pkl'

open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()

del(open_file, t, file_name, sample_list)

# Incremental training for random wiki
t = time()
sent_wiki_random = [row.split() for row in random_wiki]
phrases_wiki_random = Phrases(sent_wiki_random, progress_per=10000)
bigram_wiki_random = Phraser(phrases_wiki_random)
sentences_wiki_random = bigram_wiki_random[sent_wiki_random]
w2v_core_model.train(sentences_wiki_random, total_examples=w2v_core_model.corpus_count, epochs=30)
w2v_wiki_random_vectors = w2v_core_model.wv
print('Time to train the wiki random model: {} mins'.format(round((time() - t) / 60, 2)))

sample_list = w2v_wiki_random_vectors
file_name = '.data/Word2Vec/w2v_wiki_random_vectors.pkl'

open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()

del(sample_list, file_name, open_file, bigram_wiki_random, phrases_wiki_random, sent_wiki_random)

###########################################################################################
##### Most Frequent Words
from collections import defaultdict  # For word frequency
word_freq_wiki = defaultdict(int)
for sent in sentences_wiki:
    for i in sent:
        word_freq_wiki[i] += 1
len(word_freq_wiki)

print(sorted(word_freq_wiki, key=word_freq_wiki.get, reverse=True)[:10])
### ['also', 'first', 'one', 'time', 'two', 'new', 'use', 'work', 'year', 'may']

del(sentences_wiki, sent, i, t)

###########################################################################################
# Test wiki based core model
def w2v_model_accuracy(model):
    accuracy = model.wv.evaluate_word_analogies(datapath('questions-words.txt'))

    sum_corr = 0
    for n in range(len(accuracy[1])):
        sum_corr += len(accuracy[-1][n]['correct'])
    sum_incorr = 0
    for n in range(len(accuracy[1])):
        sum_incorr += len(accuracy[-1][n]['incorrect'])
    total = sum_corr + sum_incorr
    percent = lambda a: a / total * 100

    print('Total sentences: {}, Correct: {:.2f}%, Incorrect: {:.2f}%'.format(total, percent(sum_corr),
                                                                             percent(sum_incorr)))

w2v_model_accuracy(w2v_core_model)
print(w2v_core_model.wv.evaluate_word_pairs(datapath('wordsim353.tsv')))
