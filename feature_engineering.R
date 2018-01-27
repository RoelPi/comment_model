set.seed(5)
library(ngram) # Count characters
library(lubridate) # Dates are awesome
library(ggplot2) # <3
library(tm) # Text mining
library(SnowballC) # Needed for tm
library(data.table) # yay!
library(magrittr) # Pipes!
library(dummies)
source('topic_modeling.R')
source('sentiment_analysis.R')
us_timezone = 'America/Los_Angeles'

dt <- data.table(read.csv('data.csv',stringsAsFactors=F))
dt$article_body <- gsub("[\r\n]", "", dt$article_body)

###################
# Ad hoc features #
###################

# Article has video
dt[,article_video := grepl('Video',article_body)]

# Article length
dt[,article_word_length := sapply(gregexpr("[A-z]\\W+", article_body), length)]
dt[,article_char_length := nchar(article_body)]

# Post name length
dt[,post_name_word_length := sapply(gregexpr("[A-z]\\W+", post_name), length)]
dt[,post_name_char_length := nchar(post_name)]

# Post message length
dt[,post_message_word_length := sapply(gregexpr("[A-z]\\W+", post_message), length)]
dt[,post_message_char_length := nchar(post_message)]

# Convert to NY time
suppressWarnings(dt[,c('post_timestamp','article_timestamp') := list(strptime(post_timestamp,"%Y-%m-%d %H:%M:%S", tz = 'Europe/Brussels'),strptime(post_timestamp, "%Y-%m-%d %H:%M:%S", tz = 'Europe/Brussels'))])
dt[,c('post_timestamp_us','article_timestamp_us') := list(with_tz(post_timestamp, us_timezone),with_tz(article_timestamp,us_timezone))]

# Dummify hour of day
dt[,post_hour_us := as.factor(strftime(post_timestamp_us, "%H", tz= us_timezone))]

hours_matrix_dt <- dummy(dt$post_hour_us, sep = '_')
hours_matrix_dt <- data.table(hours_matrix_dt)

dt <- cbind(dt,hours_matrix_dt)
rm(hours_matrix_dt)

######################
# Sentiment analysis #
######################

dt <- get_sentiment(dt)

###################
# Topic discovery #
###################

dt <- get_clean_dt(dt) # Clean the article body
article_dtm <- get_dtm(dt, 0.99) # Create document-term matrix

# Get optimal topic number
# topic_number <- get_lda_topic_number(article_dtm,13,25) 
# optimal_topic_number <- as.integer(topic_number[topic_number$CaoJuan2009 == min(topic_number$CaoJuan2009),][1])
optimal_topic_number = 17

# Get topics
lda_results <- get_lda_topics(dt,article_dtm,optimal_topic_number) # This takes 10 minutes
dt <- lda_results[[1]]
article_topics <- lda_results[[2]]

article_topic_names <- c('christmas','moore_alabama','israel','scandals','women_rights','religion',
                         'flight_chaos','new_year','tax_bill','north_korea','cali_fire','fbi_investigation',
                         'trump_tweets','crime','terrorism','migration','rest')

dt[,article_topic := as.character(article_topic)]
for (i in 1:length(article_topic_names)) {
    dt[,article_topic := gsub(paste0('^',as.character(i),'$'),article_topic_names[i],article_topic)]
}

topic_matrix_dt <- dummy(dt$article_topic, sep = '_')
topic_matrix_dt <- data.table(topic_matrix_dt)

dt <- cbind(dt,topic_matrix_dt)
rm(topic_matrix, topic_matrix_dt)

###############################################
# Trigger words in facebook post name (title) #
###############################################

get_post_name_dtm_tf <- function(dt) {
    post_name_corpus <- dt$post_name %>% VectorSource() %>% Corpus()
    
    post_name_corpus_clean <- post_name_corpus %>% 
        # convert to lower case
        tm_map(content_transformer(tolower)) %>% 
        # remove numbers and punctuations
        tm_map(content_transformer(function(x) gsub("[^[:alpha:][:space:]]*", "", x))) %>% 
        # remove stopwords
        tm_map(removeWords, union(stopwords('english'),remove_words)) %>% 
        # remove extra whitespaces
        tm_map(stripWhitespace)
    
    # Create document-termmatrix
    post_name_dtm <- post_name_corpus_clean %>% 
        DocumentTermMatrix(control = list(wordLengths = c(1, Inf)))
    
    # Remove sparse words
    post_name_dtm_tf <- weightTf(post_name_dtm)
    post_name_dtm
}

post_name_dtm_tf <- get_post_name_dtm_tf(dt)
post_name_word_sums <- head(sort(colSums(as.matrix(post_name_dtm_tf)),decreasing=T),50)

# Manually curating the trigger words
post_name_trigger_words <- intersect(names(post_name_word_sums),
                                     c('trump','opinion','christmas', 'korea','tax','california','sexual','senate','fire','war','israel','clinton','russia','alabama'))

for (i in 1:length(post_name_trigger_words)) {
    dt[, c(paste0('post_name_word_',post_name_trigger_words[i])) := grepl(post_name_trigger_words[i],post_name,ignore.case=T)]
}

write.csv(dt,'data_featured.csv',sep=';')
