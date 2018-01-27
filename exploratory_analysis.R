#############
# Sentiment #
#############

dt_sentiment <- melt(dt_backup,id.vars='post_id',measure.vars=c('article_sentiment_anger',
                                                                'article_sentiment_anticipation',
                                                                'article_sentiment_disgust',
                                                                'article_sentiment_fear',
                                                                'article_sentiment_joy',
                                                                'article_sentiment_positive',
                                                                'article_sentiment_negative',
                                                                'article_sentiment_sadness',
                                                                'article_sentiment_surprise',
                                                                'article_sentiment_trust'))

dt_post_target <- dt_backup[,.(post_id,comment_count)]
dt_sentiment <- merge(dt_sentiment,dt_post_target,by="post_id")
dt_sentiment <- dt_sentiment[,lg_comment_count := log(comment_count)]
g_sentiment <- ggplot(dt_sentiment,aes(x=value,y=log(comment_count),fill=variable,col=variable)) + 
    geom_point(shape = 1) +
    geom_smooth(method='lm') +
    facet_wrap(~variable,ncol=4) +
    ylab('comment count (natural log)') +
    xlab('NRC sentiment matches (normalized to total matches)') +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_sentiment

#########
# Topic #
#########

dt_topic <- melt(dt_backup,id.vars='post_id',measure.vars=c(c('article_topic_christmas','article_topic_moore_alabama','article_topic_israel','article_topic_scandals','article_topic_women_rights','article_topic_religion',
                                                                  'article_topic_flight_chaos','article_topic_new_year','article_topic_tax_bill','article_topic_north_korea','article_topic_cali_fire','article_topic_fbi_investigation',
                                                                  'article_topic_trump_tweets','article_topic_crime','article_topic_terrorism','article_topic_migration','article_topic_rest')))
dt_topic <- merge(dt_topic,dt_post_target,by="post_id")
dt_topic <- dt_topic[,lg_comment_count := log(comment_count)]
dt_topic$value <- as.factor(dt_topic$value)
g_topic <- ggplot(dt_topic,aes(x=value,y=log(comment_count),fill=variable)) + 
    geom_boxplot() +
    facet_wrap(~variable,ncol=6) +
    ylab('comment count (natural log)') +
    xlab('topic (dummy match)') +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_topic

#################
# Trigger words #
#################

dt_triggers <- melt(dt_backup,id.vars='post_id',measure.vars=c('post_name_word_trump','post_name_word_opinion','post_name_word_christmas', 'post_name_word_korea','post_name_word_tax','post_name_word_california','post_name_word_sexual','post_name_word_senate','post_name_word_fire','post_name_word_war','post_name_word_israel','post_name_word_clinton','post_name_word_russia','post_name_word_alabama'))
dt_triggers <- merge(dt_triggers,dt_post_target,by="post_id")
dt_triggers <- dt_triggers[,lg_comment_count := log(comment_count)]
dt_triggers$value <- as.factor(dt_triggers$value)
g_triggers <- ggplot(dt_triggers,aes(x=as.factor(as.numeric(value)-1),y=log(comment_count),fill=variable)) + 
    geom_boxplot() +
    facet_wrap(~variable,ncol=4) +
    ylab('comment count (natural log)') +
    xlab('term in post title (dummy match)') +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_triggers

#########
# Hour  #
#########

g_hour <- ggplot(dt_backup,aes(x=as.factor(post_hour_us),y=log(comment_count), fill = as.factor(post_hour_us))) + 
    geom_boxplot() +
    ylab('comment count (natural log)') +
    xlab('hour of the day') +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_hour

##################
# Early signals  #
##################

p <- data.table(read.csv('all_fox_posts.csv', stringsAsFactors=F))
p <- data.table(p[2:nrow(p),])
p$X <- NULL
p$timestamp <- substr(p$timestamp,1,nchar(p$timestamp)-5)
p[,timestamp:=strptime(timestamp,"%Y-%m-%dT%H:%M:%S", tz='Europe/Brussels')]
p[,date:=strptime(date,'%Y-%m-%d',tz='Europe/Brussels')]

c <- data.table(read.csv('all_fox_comments.csv', stringsAsFactors=F))
c$X <- NULL
c$comment_timestamp <- substr(c$comment_timestamp,1,nchar(c$comment_timestamp)-4)
c[,comment_timestamp := strptime(comment_timestamp,"%Y-%m-%dT%H:%M:%S", tz='Europe/Brussels')]

# Select columns that have been placed in first 24 hours.
pc <- data.table(merge(p,c,by='post_id'))
pc[,firstDay := (comment_timestamp - timestamp < 60*60)]
pc_full <- pc
pc <- pc[firstDay == T]

# Aggregate comments and count per post_id
c_condensed <- pc[,.(comment_count = length(comment_id)),by=post_id]
c_condensed <- c_condensed[grep('[0-9].*_[0-9].*',c_condensed$post_id),]

dt_early <- merge(dt_backup,c_condensed,by='post_id',all.x=T)

g_early <- ggplot(dt_early, aes(x=log(comment_count.y),y=log(comment_count.x),col=log(comment_count.x))) + 
    geom_point() +
    geom_smooth(method='lm') +
    ylab('comment count after 24 hours (natural log)') +
    xlab('comment count after 1 hour(natural log)') +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_early

pc_full[,time_diff := round(as.numeric(comment_timestamp - timestamp) / 60,0)]
pc_full[,time_diff_hour := round(time_diff / 60,0)]
pc_full_by_hour <- pc_full[time_diff_hour >= 0 & time_diff_hour < 24]
pc_full_by_hour <- pc_full_by_hour[,.(comment_count= length(comment_id)),by=time_diff_hour]
options(scipen=10000)
pc_full_by_hour <- pc_full_by_hour[order(time_diff_hour)]
g_order <- ggplot(pc_full_by_hour,aes(x=as.factor(time_diff_hour),y = comment_count, fill=comment_count)) +
    geom_bar(stat='identity') +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24)) +
    xlab('hours since posting') +
    ylab('comment count')
g_order

pc_cumsum <- pc_full[,comments := 1]
pc_cumsum <- pc_cumsum[order(post_id,timestamp,time_diff)]
pc_cumsum <- pc_cumsum[,.(comment_sum = sum(comments)),by=.(post_id,time_diff)]
pc_cumsum <- pc_cumsum[,.(time_diff = time_diff, comment_cumsum = cumsum(comment_sum)),by=.(post_id)]
pc_cumsum <- pc_cumsum[time_diff >= 0]
g_cumsum <- ggplot(pc_cumsum,aes(x=time_diff,y=comment_cumsum,col=post_id)) +
    geom_line(size=1,alpha=0.4) +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24)) +
    scale_x_continuous(limits=c(0,2880)) +
    xlab('minutes since posting (first 48 hours)') +
    ylab('cumulative comment count')
g_cumsum