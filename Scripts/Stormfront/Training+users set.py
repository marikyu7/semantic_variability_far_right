import pandas as pd
from random import sample
import pickle
import random

dfSf = pd.read_csv(r'.data/dfSf.csv')

dfSf_users = dfSf[['stormfront_self_content', 'stormfront_user']].copy()
dfSf_users['stormfront_user'] = dfSf_users['stormfront_user'].astype('category')

# pandas count distinct values in column
users_count = dfSf_users['stormfront_user'].value_counts()
users_count = users_count.drop(['revision', 'kazan188', 'Loup Gris', 'WAKE UP', 'Albus', 'FrancaisFier',
                                'Navas de Tolosa', 'obierzo', 'alber06', 'clovis','Francais de sang et de sol',
                                'montcalm', 'Vri', '0wen', 'thulesturmer'])
Sf_users = users_count
Sf_users = Sf_users.index

### select users with more than 7500 tokens
Sf_my_users = []
for u in range(len(Sf_users)):
    user_posts = dfSf_users[dfSf_users["stormfront_user"] == Sf_users[u]]
    user_tokens = 0
    for post in user_posts['stormfront_self_content']:
        post_tokens = len(post.split())
        user_tokens += post_tokens
    if user_tokens > (500*15):
        Sf_my_users.append(Sf_users[u])

Sf_my_users.remove('[]')
Sf_my_users = sample(Sf_my_users, 500)  ### choose a n of subject (how many observations? 500? maybe)

Sf_users_posts = [None] * len(Sf_my_users)
for u in range(len(Sf_my_users)):
    user_posts = pd.DataFrame({'stormfront_self_content': [], 'stormfront_user': []})  # create an empty df where I'll put user posts
    posts = dfSf_users[dfSf_users["stormfront_user"] == Sf_my_users[u]]  # subset only posts of a selected user
    posts.reset_index(inplace=True, drop=True)  # reset the index of the subset df
    user_tokens = 0  # set tokens counter to 0
    r = list(range(len(posts)))  # set the list from where you will sample
    for row in range(len(posts)):
        if user_tokens > (500 * 15):  # I don't want more the (about) 7500 tokens per user,
            continue  # so when the counter reach that level, just skip to the next user
        else:
            n = random.choice(r)  # pick a random n. from the list for random sampling
            post = posts.iloc[[n]]  # select the row that correspond to n.
            post_tokens = len(
                post['stormfront_self_content'].astype("string").to_string())  # count tokens in the comment of the selected row
            user_tokens += post_tokens  # update tokens counter
            user_posts = pd.concat([user_posts, post], ignore_index=True,
                                   sort=False)  # add the comment to the final user's comment selection
            r.remove(n)  # remove n. from the sampling list so you don't select a post/comment twice
    Sf_users_posts[u] = user_posts

sample_list = Sf_users_posts
file_name = '.data/Sf_users_posts.pkl'

open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()

del(users_count, user_tokens, user_posts, u, sample_list, row, r, posts, post_tokens, post, open_file, n, file_name)


### Start working on core df from here
lst = list(Sf_users)

Sf_core = [x for x in Sf_users if x not in Sf_my_users]

Sf_non_users_df = pd.DataFrame({'stormfront_self_content': [], 'stormfront_user': []})
for u in range(len(Sf_core)):
    non_users_posts = dfSf_users[dfSf_users["stormfront_user"] == Sf_core[u]]
    Sf_non_users_df = pd.concat([Sf_non_users_df, non_users_posts], ignore_index=True,sort=False)

Sf_core_df = pd.DataFrame({'stormfront_self_content': [], 'stormfront_user': []})
core_tokens = 0  # set tokens counter to 0
r = list(range(len(Sf_non_users_df)))  # set the list from where you will sample
for row in range(len(Sf_non_users_df)):
    if core_tokens > (500000 *3 * 15):  # I want (about) 7.500.000 tokens
        break  # so when the counter reach that level, just stop the process: I have the n. of tokens I want
    else:
        n = random.choice(r)  # pick a random n. from the list for random sampling
        post = Sf_non_users_df.iloc[[n]]  # select the row that correspond to n.
        post_tokens = len(post['stormfront_self_content'].astype("string").to_string())  # count tokens in the comment of the selected row
        core_tokens += post_tokens  # update tokens counter
        Sf_core_df = pd.concat([Sf_core_df, post], ignore_index=True,
                                   sort=False)  # add the comment to the final user's comment selection
        r.remove(n)  # remove n. from the sampling list so you don't select a post/comment twice


Sf_core_df.to_csv('.data/Sf_core_df.csv', index=False)

del(Sf_non_users_df, Sf_users, core_tokens, lst, n, non_users_posts, post, post_tokens, r, row, u)
