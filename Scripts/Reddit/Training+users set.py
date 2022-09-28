import pandas as pd
from random import sample
import pickle
import random
from time import time

dfRC = pd.read_csv(r'.data/dfRC.csv')

dfRC_users = dfRC[['body', 'author']].copy()
dfRC_users['author'] = dfRC_users['author'].astype('category')

t = time()
# pandas count distinct values in column
users_count = dfRC_users['author'].value_counts()
RC_users = users_count
RC_users = RC_users.index

### select users with more than 7500 tokens
RC_my_users = []
for u in range(len(RC_users)):
    user_posts = dfRC_users[dfRC_users["author"] == RC_users[u]]
    user_tokens = 0
    for post in user_posts['body']:
        post_tokens = len(post.split())
        user_tokens += post_tokens
    if user_tokens > (500*15):
        RC_my_users.append(RC_users[u])

RC_my_users.remove('[deleted]')
RC_my_users = sample(RC_my_users, 500)  ### choose a n of subject (how many observations? 500? maybe)
print('Time to select users: {} mins'.format(round((time() - t) / 60, 2)))

t = time()
RC_users_posts = [None] * len(RC_my_users)
for u in range(len(RC_my_users)):
    user_posts = pd.DataFrame({'body': [], 'author': []})  # create an empty df where I'll put user posts
    posts = dfRC_users[dfRC_users["author"] == RC_my_users[u]]  # subset only posts of a selected user
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
                post['body'].astype("string").to_string())  # count tokens in the comment of the selected row
            user_tokens += post_tokens  # update tokens counter
            user_posts = pd.concat([user_posts, post], ignore_index=True,
                                   sort=False)  # add the comment to the final user's comment selection
            r.remove(n)  # remove n. from the sampling list so you don't select a post/comment twice
    RC_users_posts[u] = user_posts
print('Time to create users df: {} mins'.format(round((time() - t) / 60, 2)))


t = time()
sample_list = RC_users_posts
file_name = '.data/RC_users_posts.pkl'

open_file = open(file_name, "wb")
pickle.dump(sample_list, open_file)
open_file.close()
print('Time to save users\' list: {} mins'.format(round((time() - t) / 60, 2)))

del(users_count, user_tokens, user_posts, u, sample_list, row, r, posts, post_tokens, post, open_file, n, file_name)



#####################################################################################################
### Start working on core df from here
t= time()
lst = list(RC_users)
RC_core = RC_my_users + lst
x = pd.Series(RC_core)
x.drop_duplicates(keep=False,inplace=True)
RC_core = x.tolist()

print('Time to create core\'s users list: {} mins'.format(round((time() - t) / 60, 2)))

#dfRC_users40 = dfRC_users.sample(frac=0.4, random_state=1, ignore_index = True)
RC_non_users_list = [None]*len(RC_core)
t=time()
for u in range(len(RC_core)):
    non_users_posts = dfRC_users[dfRC_users["author"] == RC_core[u]]
    RC_non_users_list[u] = non_users_posts
print('Time to generate users\' list: {} mins'.format(round((time() - t) / 60, 2)))

t = time ()
RC_non_users_df = pd.concat(RC_non_users_list)
print('Time to generate core: {} mins'.format(round((time() - t) / 60, 2)))

t = time()
RC_core_df = pd.DataFrame({'body': [], 'author': []})
core_tokens = 0  # set tokens counter to 0
r = list(range(len(RC_non_users_df)))  # set the list from where you will sample
for row in range(len(RC_non_users_df)):
    if core_tokens > (500000 * 3 * 15):  # I want (about) 7.500.000 tokens
        break  # so when the counter reach that level, just stop the process: I have the n. of tokens I want
    else:
        n = random.choice(r)  # pick a random n. from the list for random sampling
        post = RC_non_users_df.iloc[[n]]  # select the row that correspond to n.
        post_tokens = len(post['body'].astype("string").to_string())  # count tokens in the comment of the selected row
        core_tokens += post_tokens  # update tokens counter
        RC_core_df = pd.concat([RC_core_df, post], ignore_index=True,
                                   sort=False)  # add the comment to the final user's comment selection
        r.remove(n)  # remove n. from the sampling list so you don't select a post/comment twice/


RC_core_df.to_csv('.data/RC_core_df.csv', index=False)
print('Time to create core df: {} mins'.format(round((time() - t) / 60, 2)))

del()

