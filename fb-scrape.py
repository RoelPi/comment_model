# -*- coding: utf-8 -*-
"""
Created on Sat Dec 16 21:25:23 2017

@author: roel
"""

import facebook
import pandas as pd
import requests
import datetime as dt
from time import time
import os

# fbToken = open('fb_token.txt', 'r').readlines()
apiVersion = 'v2.10'


def get_all_page_posts(page_id, min_date):
    min_date = dt.datetime.strptime(min_date, '%Y-%m-%d')
    start_time = time()
    page = requests.get('https://graph.facebook.com/'+ apiVersion + '/' + page_id + '?fields=posts.limit(100)&access_token=' + fbToken).json()
    page = page['posts']
    
    types = []
    timestamps = []
    dates = []
    ids = []
    
    post_date_not_queried = True
    i = 0
    total_time = 0
    while (post_date_not_queried):
        start_time = time()
        for post in page['data']:
            post_date = dt.datetime.strptime(post['created_time'][0:10], '%Y-%m-%d')
            if ((post_date - min_date).days >= 0):
                if 'message' in post:
                    types.append('message')
                else:
                    types.append('story')
                timestamps.append(post['created_time'])
                ids.append(post['id'])
                dates.append(post['created_time'][0:10])
            else:
                post_date_not_queried = False
        page = requests.get(page['paging']['next']).json()
        i += 1
        end_time = time()
        query_time = round((end_time - start_time),1)
        total_time = round(total_time + query_time,1)
        print("Getting posts of page '%s'. | Pagination %s | %ss | Tot. time: %ss | Avg. time: %ss" % (page_id, i, str(query_time), str(total_time), str(round(total_time / i,1))))
    return pd.DataFrame({'page': page_id, 'type': types, 'timestamp': timestamps, 'date': dates, 'post_id': ids})

def enrich_page_post(post_id):
    post = requests.get('https://graph.facebook.com/'+ apiVersion + '/' + post_id + '?fields=id, caption, description, created_time, feed_targeting, is_hidden, is_published, link, message, message_tags, name, picture, shares, targeting, type&access_token=' + fbToken).json()
    if post['type'] == 'link':
        if 'message' in post:
            to_return = [post_id, post['type'], post['is_hidden'], post['is_published'], post['link'], post['name'], post['message'], post['picture']]
        else:
            to_return = [post_id, post['type'], post['is_hidden'], post['is_published'], post['link'], post['name'], float('NaN'), post['picture']]
    else:
        to_return = [post_id, post['type'], float('NaN'), float('NaN'), float('NaN'), float('NaN'), float('NaN'), float('NaN')]
    return to_return

def enrich_page_posts(posts):
    start_time = time()
    total_time = 0
    enriched_posts = []
    for i, post_id in enumerate(posts['post_id']):
        start_time = time()
        enriched_posts.append(enrich_page_post(post_id))
        end_time = time()
        query_time = round((end_time - start_time),1)
        total_time = round(total_time + query_time,1)
        print("Enriching post %s of %s. | %ss | Tot. time: %ss | Avg. time: %ss | Est. time: %ss" % ((i+1), str(len(posts)), str(query_time), str(total_time), str(round(total_time / (i+1),1)), str(round((total_time/(i+1))*(len(posts)-(i+1)),1))))
    enriched_posts_df = pd.DataFrame(enriched_posts, columns = ['post_id', 'subtype', 'hidden', 'published', 'url', 'name', 'message', 'picture_url'])
    return enriched_posts_df

def get_comments_for_post(post_id):
    comments_list = []
    comments = requests.get('https://graph.facebook.com/'+ apiVersion + '/' + post_id + '?fields=comments.limit(100)&access_token=' + fbToken).json()
    comments = comments['comments']
    
    while(True):
        if 'data' in comments:
            i_comments_list = [[post_id, comment['id'], comment['message'], comment['created_time']] for comment in comments['data']]
            comments_list.extend(i_comments_list)
            if 'paging' in comments and 'next' in comments['paging']:
                commentsBackup = comments.copy()
                comments = requests.get(comments['paging']['next']).json()
            else:
                break
        else:
            print(comments)
            what_do = input("Try again? (y/n)")
            if what_do == 'y':
                comments = requests.get(commentsBackup['paging']['next']).json()
            else:
                break
    return comments_list

def get_comments_for_posts(posts):
    post_comments = []
    total_time = 0
    for i, post_id in enumerate(posts['post_id']):
        start_time = time()
        post_comments.extend(get_comments_for_post(post_id))
        end_time = time()
        query_time = round((end_time - start_time),1)
        total_time = round(total_time + query_time,1)
        print("Got comments for post %s of %s. | %ss | Tot time: %ss | Avg. time %ss | Est. time: %s" % ((i+1), str(len(posts)), query_time, total_time, str(round(total_time/(i+1),1)), str(round((total_time/(i+1))*(len(posts)-(i+1)),1))))
    post_comments_df = pd.DataFrame(post_comments,columns = ['post_id', 'comment_id', 'comment_message', 'comment_timestamp'])
    return post_comments_df

def get_likes_for_post(post_id):
    likes_list = []
    likes = requests.get('https://graph.facebook.com/'+ apiVersion + '/' + post_id + '/reactions?access_token=' + fbToken + '&limit=100').json()

    while(True):
        i_likes_list = [[post_id, like['id'], like['name'], like['type']] for like in likes['data']]
        likes_list.extend(i_likes_list)
        if 'paging' in likes and 'next' in likes['paging']:
            likes = requests.get(likes['paging']['next']).json()
        else:
            break
    return likes_list

def get_likes_for_posts(posts):
    post_likes = []
    total_time = 0
    for i, post_id in enumerate(posts['post_id']):
        start_time = time()
        post_likes.extend(get_likes_for_post(post_id))
        end_time = time()
        query_time = round((end_time - start_time),1)
        total_time = round(total_time + query_time,1)
        print("Got likes for post %s of %s. | %ss | Tot time: %ss | Avg. time %ss | Est. time: %s" % ((i+1), str(len(posts)), query_time, total_time, str(round(total_time/(i+1),1)), str(round((total_time/(i+1))*(len(posts)-(i+1)),1))))
    post_likes_df = pd.DataFrame(post_likes,columns = ['post_id', 'reaction_id', 'name', 'reaction_type'])
    return post_likes_df

def get_all_for_date(posts, folder, date, c = True, l = True):
    print('Start collection of data for %s' % date)
    start_time = time()
    dated_posts = posts[posts.date == date]
    enriched_posts = enrich_page_posts(dated_posts)
    link_posts = enriched_posts[enriched_posts.subtype == 'link']
    posts = link_posts.join(dated_posts.set_index('post_id'), on = 'post_id', how = 'left', lsuffix = "_l", rsuffix = "_r")
    posts.to_csv(folder + '/' + date + "_posts.csv", sep=";", index=False)
    if (c == True):
        comments = get_comments_for_posts(link_posts)
        comments.to_csv(folder + '/' + date + "_comments.csv", sep=";", index=False)
    if (l == True):
        likes = get_likes_for_posts(link_posts)
        likes.to_csv(folder + '/' + date + "_likes.csv", sep=";", index=False)
    end_time = time()
    query_time = round((end_time - start_time),1)
    print('Finish collection of data for %s. Total time: %s.' % (date,query_time))

def get_all_for_dates(posts, folder, start_date, end_date, c = True, l = True):
    if not os.path.exists(folder):
        os.makedirs(folder)
    start_date_dt = dt.datetime.strptime(start_date,'%Y-%m-%d')
    end_date_dt = dt.datetime.strptime(end_date,'%Y-%m-%d')
    delta_dt = end_date_dt - start_date_dt
    date_list = [dt.datetime.strftime(start_date_dt + dt.timedelta(days = i), '%Y-%m-%d') for i in range(delta_dt.days + 1)]
    for date in date_list:
        get_all_for_date(posts, folder, date, c = True, l = False)

start_date = '2018-01-09'
end_date = '2018-01-10'

get_all_for_dates(get_all_page_posts(page_id = 'cnn', min_date = start_date),'CNN',start_date, end_date, l=False)
get_all_for_dates(get_all_page_posts(page_id = 'FoxNews', min_date = start_date),'Fox',start_date, end_date, l=False)
