library(topicmodels) # LDA
library(parallel) # For LDA tuning
library(tm) # DTM making
library(lsa) # better than LDA
library(ldatuning) # Tune number of LDA topics
library(data.table) # yay!

remove_words = c('news','us','said','follow','will','told','also','twitter','report')

get_clean_dt <- function(dt) {
    dt[,article_body := gsub('[^[:alpha:][:space:]]*', '', article_body)] # Remove numeric stuff
    dt[,article_body := gsub('^close','',article_body)] # Remove word 'close' as a starting word in body
    dt[,article_body := gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", article_body)] # Remove URLs
    # dt[,article_body := trimws(article_body,"b")] # Remove trailing spaces
    dt[,article_body := gsub('^Video','',dt$article_body)] # Remove 'video' as starting word in body
    dt
}

get_dtm <- function(dt, sparseness = 0.99) {
    article_corpus <- dt$article_body %>% VectorSource() %>% Corpus()
    
    article_corpus_clean <- article_corpus %>% 
        # convert to lower case
        tm_map(content_transformer(tolower)) %>% 
        # remove numbers and punctuations
        tm_map(content_transformer(function(x) gsub("[^[:alpha:][:space:]]*", "", x))) %>% 
        # remove stopwords
        tm_map(removeWords, union(stopwords('english'),remove_words)) %>% 
        # remove extra whitespaces
        tm_map(stripWhitespace)
    
    article_corpus_complete <- article_corpus_clean %>%
        # Stem documents
        tm_map(stemDocument)
    
    # Create document-termmatrix
    article_dtm <- article_corpus_complete %>% 
        DocumentTermMatrix(control = list(wordLengths = c(1, Inf)))
    
    # Remove sparse words
    article_dtm <- article_dtm %>% removeSparseTerms(sparse=sparseness)
    article_dtm
}

get_lda_topic_number <- function(dtm,tryTopicsLow=5,tryTopicsHigh=25) {
    result <- FindTopicsNumber(
        dtm,
        topics = seq(from = tryTopicsLow, to = tryTopicsHigh, by = 1),
        metrics = c("CaoJuan2009"),
        method = "Gibbs",
        control = list(seed = 13654, verbose=T, thin = 500, iter=2000, burnin = 4000, nstart= 1, best=T),
        mc.cores = 3L,
        verbose = TRUE
    )
    result
}

get_lda_topics <- function(dt,dtm,topic_number=10) {
    # Add weight to terms
    article_dtm_tf <- weightTf(dtm)
    article_body_lda <- LDA(article_dtm_tf, method='Gibbs', k=topic_number, control = list(seed=list(13654,2874,32,4152,500), verbose=T, thin = 500, iter=2000, burnin = 4000, nstart=5, best=T))
    topics <- terms(article_body_lda, 25)
    
    topics_dt <- data.table(as.factor(topics(article_body_lda)))
    colnames(topics_dt) <- c('article_topic')
    dt <- cbind(dt,topics_dt)
    list(dt,topics)
}


