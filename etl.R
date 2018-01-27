library(data.table)
library(rvest)

# a1 <- read.csv(file='data/fox_articles.csv', sep=';',stringsAsFactors=F)
# a2 <- read.csv(file='data/fox_articles_2.csv', sep=';',stringsAsFactors=F)
# a <- data.table(rbind(a1,a2))
# rm(a1, a2)

# a = a[a$author != '',] # Remove Fox Business

# for (i in 1:nrow(a)) {
#     if (a[i]$title == '') {
#         title <- html_text(html_nodes(read_html(a[i]$url),'h1.headline'))
#         a[i]$title = title
#     }
# }

# write.csv(a,file="all_fox_articles.csv",sep=";")

#####################
# Load article data #
#####################

a <- data.table(read.csv('all_fox_articles.csv', stringsAsFactors=F))
a$date <- substr(a$date,1,nchar(a$date)-6)
a[,date:=strptime(date,"%Y-%m-%dT%H:%M:%S", tz='Europe/Brussels')]

a[,c(1,2)] <- NULL

##################
# Load post data #
##################

# p <- ''
# dir <- 'data/comments/'
# files <- dir(dir, pattern="posts.csv")
# for (i in 1:length(files)) {
#     print(paste0("Reading: ", dir, files[i]))
#     file <- read.csv(paste0(dir, files[i]), header=T, stringsAsFactors=F, sep=';', row.names=NULL)
#     p <- rbind(p, file)
# }
# write.csv(p,file="all_fox_posts.csv",sep=";")

p <- data.table(read.csv('all_fox_posts.csv', stringsAsFactors=F))
p <- data.table(p[2:nrow(p),])
p$X <- NULL
p$timestamp <- substr(p$timestamp,1,nchar(p$timestamp)-5)
p[,timestamp:=strptime(timestamp,"%Y-%m-%dT%H:%M:%S", tz='Europe/Brussels')]
p[,date:=strptime(date,'%Y-%m-%d',tz='Europe/Brussels')]

#####################
# Load comment data #
#####################

# c <- ''
# dir <- 'data/comments/'
# files <- dir(dir, pattern="comments.csv")
# for (i in 1:length(files)) {
#    print(paste0("Reading: ", dir, files[i]))
#    file <- read.csv(paste0(dir, files[i]), header=T, stringsAsFactors=F, sep=';',row.names=NULL)
#     c <- rbind(c, file)
# }
# write.csv(c,file="all_fox_comments.csv",sep=";")

c <- data.table(read.csv('all_fox_comments.csv', stringsAsFactors=F))
c$X <- NULL
c$comment_timestamp <- substr(c$comment_timestamp,1,nchar(c$comment_timestamp)-4)
c[,comment_timestamp := strptime(comment_timestamp,"%Y-%m-%dT%H:%M:%S", tz='Europe/Brussels')]

# Select columns that have been placed in first 24 hours.
pc <- data.table(merge(p,c,by='post_id'))
pc[,firstDay := (comment_timestamp - timestamp < 60*60*24)]
pc <- pc[firstDay == T]

# Aggregate comments and count per post_id
c_condensed <- pc[,.(comment_count = length(comment_id)),by=post_id]
c_condensed <- c_condensed[grep('[0-9].*_[0-9].*',c_condensed$post_id),]
rm(pc)

df <- merge(x=a,y=p,by='post_id',all.x=T)
df <- merge(x=df,y=c_condensed,by='post_id', all.x=T)
df[,c('subtype','hidden','published','page'):=NULL]
setnames(df,
         old=c('author',
                'body',
                'date.x',
                'title',
                'date.y',
                'url.x',
                'url.y',
                'name',
                'message',
                'picture_url',
                'type',
                'timestamp'),
         new=c('article_author',
                'article_body',
                'article_timestamp',
                'article_title',
                'post_date',
                'article_url',
                'post_url',
                'post_name',
                'post_message',
                'post_picture_url',
                'post_type',
               'post_timestamp'))
rm(a,c,c_condensed,p)
write.csv(df,'data.csv',sep=';',row.names=F)
