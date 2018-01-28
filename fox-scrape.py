# -*- coding: utf-8 -*-
"""
Created on Sun Dec 24 16:35:25 2017

@author: roel
"""
import pandas as pd
import urllib3 as u
import bs4 as bs
import glob
import os
import time
import requests
import sys

u.disable_warnings()

def scrape_article_from_fox(post_id,url):
    http = u.PoolManager()
    # page = http.request('GET', url)
    page = requests.get(url, headers={'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36'})
    parsedPage = bs.BeautifulSoup(page.text,'html.parser')
    
    # Title
    title_sel = parsedPage.select_one('.article-header h1.headline.head1')
    if title_sel:
        title = title_sel.text.strip()
    elif parsedPage.select_one('#page-title'):
        title = parsedPage.select_one('#page-title').text.strip()
    else:
        title = float('NaN')
    
    # Body
    body_sel = parsedPage.select_one('.article-body')
    if body_sel:
        body = body_sel.text.strip()
    elif parsedPage.select_one('.articleBody'):
        body = parsedPage.select_one('.articleBody').text.strip()
    else:
        body = float('NaN')

    # Date
    publish_date_sel = parsedPage.select_one('.article-date time')
    if publish_date_sel:
        publish_date = publish_date_sel['data-time-published']
    elif parsedPage.select_one('#date'):
        publish_date = parsedPage.select_one('#date')
    else:
        publish_date = float('NaN')
    
    # Author
    author_sel = parsedPage.select_one('.author-byline')
    if author_sel:
        author = author_sel.text.strip()
        author = ' '.join(author.split()).replace('By ', '').replace('| Fox News', '')
    else:
        author = float('NaN')
        
    print('Fox | ' + str(post_id) + ' | "' + str(title) + '" scraped successfully.')
    return {'post_id': post_id, 'url': url, 'date': publish_date, 'author': author, 'title': title, 'body': body}

def scrape_articles_from_fox(post_ids,urls):
    fox_articles = []    
    for i, url in enumerate(urls):
        while True:
            try:
                fox_articles.append(scrape_article_from_fox(post_ids[i], url))
            except KeyError:
                print('Key Error discovered.')
                break
            except KeyboardInterrupt:
                sys.exit('Interrupted by Keyboard.')
                break
            except:
                print('Unknown error. Trying again in 60 seconds.')
                time.sleep(60)
                fox_articles.append(scrape_article_from_fox(post_ids[i], url))
                continue
            break
        time.sleep(3)
    return fox_articles


def read_articles_for_files_in_folder(folder):
    files = os.listdir(folder)
    files = [file for file in files if 'posts' in file]
    for i, file in enumerate(files):
        if i <= 1:
            article_list = pd.read_csv(folder + '/' + file, encoding = 'latin1', sep=';', error_bad_lines=False)
        else:
            article_list = article_list.append(pd.read_csv(folder + '/' + file, encoding = 'latin1',  sep=';', error_bad_lines=False))
    post_ids = list(article_list['post_id'])
    urls = list(article_list['url'])
    articles = scrape_articles_from_fox(post_ids,urls)
    return articles

os.chdir('C:/Users/roel/Documents/GitHub/comment_scraper/')
Fox_article_list = read_articles_for_files_in_folder('Fox')
df = pd.DataFrame.from_dict(list(Fox_article_list))
df.to_csv('fox_articles.csv',sep=";")
