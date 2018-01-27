library(dplyr)
library(tidytext)
get_sentiment <- function(data_table) {
    docs <- dt$article_body
    dt <- data_frame(text=docs,article=1:length(docs))
    dt <- dt %>% unnest_tokens(word,text)
    dt <- dt %>% inner_join(get_sentiments('nrc'))
    dt <- data.table(dt)
    dt_count <- dt[,.(sent_count=length(word)),by=.(article)]
    dt <- dt[,.(sent_count=length(word)),by=.(article,sentiment)]
    dt <- merge(dt, dt_count,by = 'article')
    dt <- dt[,sent_count := sent_count.x / sent_count.y]
    dt <- dcast(dt,article~ sentiment, fill=0, value.var='sent_count')
    dt$article <- NULL
    colnames(dt) <- paste0('article_sentiment_',colnames(dt))
    dt <- data.table(cbind(data_table,dt))
    dt
}