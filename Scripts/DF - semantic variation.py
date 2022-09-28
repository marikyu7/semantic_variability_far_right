import pandas as pd

cosine_RC_users_core = pd.read_csv(r'.data/Similarity measures/cosine_RC_users_core.csv')
cosine_Sf_users_core = pd.read_csv(r'.data/Similarity measures/cosine_Sf_users_core.csv')
cosine_RC_RC_users = pd.read_csv(r'.data/Similarity measures/cosine_RC_RC_users.csv')
cosine_Sf_Sf_users = pd.read_csv(r'.data/Similarity measures/cosine_Sf_Sf_users.csv')
cosine_Sf_RC_users = pd.read_csv(r'.data/Similarity measures/cosine_Sf_RC_users.csv')

overlap_RC_users_core = pd.read_csv(r'.data/Similarity measures/overlap_RC_users_core.csv')
overlap_Sf_users_core = pd.read_csv(r'.data/Similarity measures/overlap_Sf_users_core.csv')
overlap_RC_RC_users = pd.read_csv(r'.data/Similarity measures/overlap_RC_RC_users.csv')
overlap_Sf_Sf_users = pd.read_csv(r'.data/Similarity measures/overlap_Sf_Sf_users.csv')
overlap_Sf_RC_users = pd.read_csv(r'.data/Similarity measures/overlap_Sf_RC_users.csv')

RC_word_frequency = pd.read_csv(r'.data/Similarity measures/RC_word_frequency.csv')
Sf_word_frequency = pd.read_csv(r'.data/Similarity measures/Sf_word_frequency.csv')
word_freq_RC_RC_users = pd.read_csv('.data/Similarity measures/word_freq_RC_RC_users.csv')
word_freq_Sf_Sf_users = pd.read_csv('.data/Similarity measures/word_freq_Sf_Sf_users.csv')
word_freq_Sf_RC_users = pd.read_csv('.data/Similarity measures/word_freq_Sf_RC_users.csv')

words = ['white', 'black', 'race', 'peopl', 'nation', 'immigr'] #brown

######################################################################################################################
#### Let's start with users'ID ####
ID = []
for user in range(len(cosine_RC_users_core)):
    for word in range(len(words)):
        ID.append(cosine_RC_users_core['ID'][user])
for user in range(len(cosine_Sf_users_core)):
    for word in range(len(words)):
        ID.append(cosine_Sf_users_core['ID'][user])
del(user, word)

for user in range(len(cosine_RC_RC_users)):
    for word in range(len(words)):
        ID.append(cosine_RC_RC_users['ID'][user])
for user in range(len(cosine_Sf_Sf_users)):
    for word in range(len(words)):
        ID.append(cosine_Sf_Sf_users['ID'][user])
del(user, word)

for user in range(len(cosine_Sf_RC_users)):
    for word in range(len(words)):
        ID.append(cosine_Sf_RC_users['ID'][user])
del(user, word)


#### User 1 ####
user_1 = []
for user in range(len(cosine_RC_users_core)):
    for word in range(len(words)):
        user_1.append(cosine_RC_users_core['ID'][user])
for user in range(len(cosine_Sf_users_core)):
    for word in range(len(words)):
        user_1.append(cosine_Sf_users_core['ID'][user])
del(user, word)

for user in range(len(cosine_RC_RC_users)):
    for word in range(len(words)):
        user_1.append(cosine_RC_RC_users['user 1'][user])
for user in range(len(cosine_Sf_Sf_users)):
    for word in range(len(words)):
        user_1.append(cosine_Sf_Sf_users['user 1'][user])
del(user, word)

for user in range(len(cosine_Sf_RC_users)):
    for word in range(len(words)):
        user_1.append(cosine_Sf_RC_users['user 1'][user])
del(user, word)


#### User 2 ####
user_2 = []
for user in range(len(cosine_RC_users_core)):
    for word in range(len(words)):
        user_2.append('wiki')
for user in range(len(cosine_Sf_users_core)):
    for word in range(len(words)):
        user_2.append('wiki')
del(user, word)

for user in range(len(cosine_RC_RC_users)):
    for word in range(len(words)):
        user_2.append(cosine_RC_RC_users['user 2'][user])
for user in range(len(cosine_Sf_Sf_users)):
    for word in range(len(words)):
        user_2.append(cosine_Sf_Sf_users['user 2'][user])
del(user, word)

for user in range(len(cosine_Sf_RC_users)):
    for word in range(len(words)):
        user_2.append(cosine_Sf_RC_users['user 2'][user])
del(user, word)


#### Cosine similarity ####
cosine_similarity = []
for user in range(len(cosine_RC_users_core)):
    for word in range(len(words)):
        cosine_similarity.append(cosine_RC_users_core.iloc[user, word+1])
for user in range(len(cosine_Sf_users_core)):
    for word in range(len(words)):
        cosine_similarity.append(cosine_Sf_users_core.iloc[user, word+1])
del(user, word)

for user in range(len(cosine_RC_RC_users)):
    for word in range(len(words)):
        cosine_similarity.append(cosine_RC_RC_users.iloc[user, word+3])
for user in range(len(cosine_Sf_Sf_users)):
    for word in range(len(words)):
        cosine_similarity.append(cosine_Sf_Sf_users.iloc[user, word+3])
del(user, word)

for user in range(len(cosine_Sf_RC_users)):
    for word in range(len(words)):
        cosine_similarity.append(cosine_Sf_RC_users.iloc[user, word+3])
del(user, word)


#### Neighborhood overlap ####
neighborhood_overlap = []
for user in range(len(overlap_RC_users_core)):
    for word in range(len(words)):
        neighborhood_overlap.append(overlap_RC_users_core.iloc[user, word+1])
for user in range(len(overlap_Sf_users_core)):
    for word in range(len(words)):
        neighborhood_overlap.append(overlap_Sf_users_core.iloc[user, word+1])
del(user, word)

for user in range(len(overlap_RC_RC_users)):
    for word in range(len(words)):
        neighborhood_overlap.append(overlap_RC_RC_users.iloc[user, word+3])
for user in range(len(overlap_Sf_Sf_users)):
    for word in range(len(words)):
        neighborhood_overlap.append(overlap_Sf_Sf_users.iloc[user, word+3])
del(user, word)

for user in range(len(overlap_Sf_RC_users)):
    for word in range(len(words)):
        neighborhood_overlap.append(overlap_Sf_RC_users.iloc[user, word+3])
del(user, word)


#### Word measured ####
semantic_concept = []
for user in range(len(cosine_RC_users_core)):
    for w in words:
        semantic_concept.append(w)
for user in range(len(cosine_Sf_users_core)):
    for w in words:
        semantic_concept.append(w)
del(user, w)

for user in range(len(cosine_RC_RC_users)):
    for w in words:
        semantic_concept.append(w)
for user in range(len(cosine_Sf_Sf_users)):
    for w in words:
        semantic_concept.append(w)
del(user, w)

for user in range(len(cosine_Sf_RC_users)):
    for w in words:
        semantic_concept.append(w)
del(user, w)

# Word frequency
word_freq = []
for user in range(len(RC_word_frequency)):
    for word in range(len(words)):
        word_freq.append(RC_word_frequency.iloc[user, word])
for user in range(len(Sf_word_frequency)):
    for word in range(len(words)):
        word_freq.append(Sf_word_frequency.iloc[user, word])
del(user, word)

for user in range(len(word_freq_RC_RC_users)):
    for word in range(len(words)):
        word_freq.append(word_freq_RC_RC_users.iloc[user, word+3])
for user in range(len(word_freq_Sf_Sf_users)):
    for word in range(len(words)):
        word_freq.append(word_freq_Sf_Sf_users.iloc[user, word+3])
del(user, word)

for user in range(len(word_freq_Sf_RC_users)):
    for word in range(len(words)):
        word_freq.append(word_freq_Sf_RC_users.iloc[user, word+3])
del(user, word)


#### Forum ####
forum = []
for user in range(len(cosine_RC_users_core)):
    for word in range(len(words)):
        forum.append('RC')
for user in range(len(cosine_Sf_users_core)):
    for word in range(len(words)):
        forum.append('Sf')
del(user, word)

for user in range(len(cosine_RC_RC_users)):
    for word in range(len(words)):
        forum.append('RC')
for user in range(len(cosine_Sf_Sf_users)):
    for word in range(len(words)):
        forum.append('Sf')
del(user, word)

for user in range(len(cosine_Sf_RC_users)):
    for word in range(len(words)):
        forum.append(None)
del(user, word)

#### Inter/Intra ####
interaction = []
for user in range(len(cosine_RC_users_core)):
    for word in range(len(words)):
        interaction.append(None)
for user in range(len(cosine_Sf_users_core)):
    for word in range(len(words)):
        interaction.append(None)
del(user, word)

for user in range(len(cosine_RC_RC_users)):
    for word in range(len(words)):
        interaction.append('intra')
for user in range(len(cosine_Sf_Sf_users)):
    for word in range(len(words)):
        interaction.append('intra')
del(user, word)

for user in range(len(cosine_Sf_RC_users)):
    for word in range(len(words)):
        interaction.append('inter')
del(user, word)


# Put everything together
semantic_variation = pd.DataFrame({'ID': ID, 'user 1': user_1, 'user 2': user_2, 'cosine similarity':cosine_similarity,
                                   'neighborhood overlap':neighborhood_overlap, 'word': semantic_concept,
                                   'word frequency': word_freq, 'forum': forum, 'inter/intra': interaction})
del(cosine_similarity, ID, neighborhood_overlap, user_1, user_2, semantic_concept, word_freq, forum, interaction)


semantic_variation['word'] = semantic_variation['word'].astype('category')
semantic_variation['forum'] = semantic_variation['forum'].astype('category')
semantic_variation['inter/intra'] = semantic_variation['inter/intra'].astype('category')

semantic_variation.to_csv('.data/semantic_variation.csv', index=False)















