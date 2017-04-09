setwd('D:/_ai_camp/transfergo')

rm(list=ls())
# load data

d <- read.csv(file='data.csv', header=T, sep=',')
dim(d)
# fix data
names(d)
names(d) = c('score', 'country', 'transactions','language', 'is_referred', 
             'first_booking', 'address_status', 'identity_status', 'referrals')
names(d)
d$country = as.factor(d$country)
d$language = as.factor(d$language)
d$is_referred= as.factor(d$is_referred)
d$address_status = as.factor(d$address_status)
d$identity_status = as.factor(d$identity_status)

# missing values
library(mice)
md.pattern(d)
# only 6 country and 7 first_booking values are missing. We will remove missing values
d <- d[complete.cases(d),]
dim(d)



sum (d$score == 10)/nrow(d)

# analysis

summary(d)
hist (d$score)

plot(d$score ~ d$country)
plot(d$score ~ d$language)
plot(d$score ~ d$transactions)
plot(d$score ~ d$first_booking)
plot(d$score ~ d$is_referred)

hist(d$score[d$is_referred == 0])
hist(d$score[d$is_referred == 1])

plot(d$score ~ d$referrals)

# random forest
if (!require('randomForest')) install.packages('randomForest')
require(randomForest)
set.seed(42)
fit=randomForest(factor(score)~., data=d)
saveRDS(fit, 'randomForest_fit.RDS')
print(fit)
varImpPlot(fit)
importance(fit)


fit2=randomForest(factor(score)~., data=d,)

head(d[,2:9])


# xgboost
require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')
require('vcd')

# prepare data
summary(d)

dd <- d[,2:9]

sparse_matrix <- sparse.model.matrix(~., data = dd)
head(sparse_matrix)
dim(sparse_matrix)

summary(as.factor(d$score))

labels = d$score/10
bst <- xgboost(data = sparse_matrix, label = labels, max.depth = 13,
               eta = 1, nthread = 1, nround = 200,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)


# second model
labels2 = d$score <5

bst2 <- xgboost(data = sparse_matrix, label = labels2, max.depth = 13,
               eta = 1, nthread = 1, nround = 200,objective = "binary:logistic")

importance2 <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst2)
head(importance2)

