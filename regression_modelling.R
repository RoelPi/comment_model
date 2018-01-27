library(ggplot2) # <3
library(data.table) # yay!
library(magrittr) # Pipes!
library(glmnet) # Cowboy lassos
library(Metrics) # easy sse
library(rpart) # Regression trees
library(leaps)
set.seed(03031988)

dt <- data.table(read.csv('data_featured.csv',stringsAsFactors=F))
suppressWarnings(dt[,c('post_timestamp','article_timestamp') := list(strptime(post_timestamp,"%Y-%m-%d %H:%M:%S", tz = 'Europe/Brussels'),strptime(post_timestamp, "%Y-%m-%d %H:%M:%S", tz = 'Europe/Brussels'))])

dt$X <- NULL # I forgot row.names
dt[,target:=log(comment_count)]
dt_backup <- dt
dt_backup <- dt_backup[order(-target)]
dt <- dt[,.(article_video, article_word_length,article_char_length,post_name_word_length,post_name_char_length,
      post_message_word_length,post_message_char_length,
      # Sentiments in the model
      article_sentiment_anger,article_sentiment_anticipation,article_sentiment_disgust,article_sentiment_fear,
      article_sentiment_joy,article_sentiment_positive,article_sentiment_negative,
      article_sentiment_sadness,article_sentiment_surprise,article_sentiment_trust,
      # Post time in the model
      post_hour_us_01,post_hour_us_02,post_hour_us_03,
      post_hour_us_04,post_hour_us_05,post_hour_us_06,post_hour_us_07,post_hour_us_08,post_hour_us_09,post_hour_us_10,
      post_hour_us_11,post_hour_us_12,post_hour_us_13,post_hour_us_14,post_hour_us_15,post_hour_us_16,post_hour_us_17,
      post_hour_us_18,post_hour_us_19,post_hour_us_20,post_hour_us_21,post_hour_us_22,post_hour_us_23,
      # Article topics in the model
      article_topic_christmas,article_topic_moore_alabama,article_topic_israel,article_topic_scandals,article_topic_women_rights,
      article_topic_religion,article_topic_flight_chaos,article_topic_new_year,article_topic_tax_bill,article_topic_north_korea,
      article_topic_cali_fire,article_topic_fbi_investigation,article_topic_trump_tweets,article_topic_crime,article_topic_terrorism,
      article_topic_migration,article_topic_rest,
      # Trigger words in the model
      post_name_word_trump,post_name_word_opinion,post_name_word_christmas,post_name_word_korea,
      post_name_word_tax,post_name_word_california,post_name_word_sexual,post_name_word_senate,post_name_word_fire,
      post_name_word_war,post_name_word_israel,post_name_word_russia,post_name_word_clinton,post_name_word_alabama,
      # Target
      target)]
dt[,1:1] <- lapply(dt[,1:1],function(x) as.factor(as.numeric(x)))
dt[,19:(ncol(dt)-1)] <- lapply(dt[,19:(ncol(dt)-1)],function(x) as.factor(as.numeric(x)))

###############################################################################################
# Train test split ############################################################################
###############################################################################################

n <- nrow(dt)
shuffled_dt <- dt[sample(n), ]
train_indices <- 1:round(0.8 * n)
dt_train <- shuffled_dt[train_indices, ]
test_indices <- (round(0.8 * n) + 1):n
dt_test <- shuffled_dt[test_indices, ]
rm(shuffled_dt, dt, train_indices, test_indices)

y <- dt_train$target
y_test <- dt_test$target
x <- dt_train
x_test <- dt_test
x$target <- NULL
x_test$target <- NULL

tss <- sum((y_test-mean(y_test))^2)

##################################################################################################
# Stepwise regression ############################################################################
##################################################################################################

predict_regsubsets <- function(object,newdata,id,...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form,newdata)
    coefi <- coef(object,id=id)
    xvars <- names(coefi)
    mat[,xvars]%*%coefi
}


step_model <- regsubsets(target~.,data=dt_train,nvmax=15,method='forward')
step_coeff <- coef(step_model,15)
step_y_test_predictions <- predict_regsubsets(step_model,dt_test,15)
y_test_comparison <- cbind(y_test,step_y_test_predictions)
step_sse <- sse(y_test, step_y_test_predictions)
step_mse <- mse(y_test, step_y_test_predictions)
step_rsq <- 1 - (sum((y_test-step_y_test_predictions )^2)/sum((y_test-mean(y_test))^2))

###############################################################################################
# Lasso regression ############################################################################
###############################################################################################

lasso_x <- model.matrix(target~.,dt_train)
lasso_x_test <- model.matrix(target~.,dt_test)

model_lasso_cv <- cv.glmnet(lasso_x,y,alpha=1)
# plot(model_lasso_cv)

lasso_coeff <- coef(model_lasso_cv,s = 'lambda.min')
lasso_y_test_predictions <- predict(model_lasso_cv,newx=lasso_x_test, s='lambda.min')
y_test_comparison <- cbind(y_test_comparison,lasso_y_test_predictions)
lasso_sse <- sse(y_test,lasso_y_test_predictions)
lasso_mse <- mse(y_test,lasso_y_test_predictions)
lasso_residuals <- lasso_y_test_predictions - y_test
lasso_rsq <- 1 - (sum((y_test-lasso_y_test_predictions )^2)/sum((y_test-mean(y_test))^2))

##############################################################################################
# Regression tree ############################################################################
##############################################################################################

model_tree <- rpart(target~.,data=dt_train,method='anova',control=rpart.control(minsplit=30, cp=0.001))
model_tree <- prune(model_tree, cp=model_tree$cptable[which.min(model_tree$cptable[,"xerror"]),"CP"])
tree_y_test_predictions <- predict(model_tree,newdata=x_test)
y_test_comparison <- cbind(y_test_comparison,tree_y_test_predictions)
tree_sse <- sse(y_test,tree_y_test_predictions)

#############################################################################################
# Compare Models ############################################################################
#############################################################################################
g_log_qq <- ggplot(dt_backup,aes(sample=target)) + 
    geom_qq() +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_log_qq

y_test_comparison <- data.table(y_test_comparison)
colnames(y_test_comparison) <- c('real','stepwise','lasso','tree')
with(y_test_comparison,plot(lasso,real))
with(y_test_comparison,abline(0,1))

g_comp <- ggplot(y_test_comparison,aes(x=lasso,y=real,col=(real>=lasso))) + 
    geom_point(size=3) + 
    geom_abline(intercept=0,slope=1) +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_comp

g_qq <- ggplot(y_test_comparison,aes(sample=lasso_residuals)) + 
    geom_qq() +
    theme(legend.position="none", axis.title.x = element_text(size=24),axis.title.y = element_text(size=24))
g_qq
