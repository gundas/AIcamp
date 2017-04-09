if (!require('Metrics')) install.packages('Metrics')
suppressMessages(library(Metrics))

if (!require('xgboost')) install.packages('xgboost')
suppressMessages(library(xgboost))

if (!require('Matrix')) install.packages('Matrix')
suppressMessages(library(Matrix))

if (!require('mice')) install.packages('mice')
suppressMessages(library(mice))

suppressMessages(library(data.table))


dd <- d[,2:9]

# convert all categorical input variables to binary representation
sparse_matrix <- sparse.model.matrix(~., data = d[,2:9])

# split training /training
set.seed(2) 
h=sample(c(0,1),prob=c(0.2,0.8),nrow(sparse_matrix),replace=T)
train=sparse_matrix[h==1,]
test=sparse_matrix[h==0,]

train_labels = d$score[h==1]
test_labels = d$score[h==0]

bst <- xgboost(data = train, label = train_labels, max.depth = 5,
               eta = 1, nthread = 1, nround = 50,objective = "multi:softmax", num_class = 11)

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

# predict
rez <- predict(bst, test)

auc(test_labels, rez)

error = test_labels - rez
sum(error == 0 )/length(error)
sum( abs(error) < 3 )/length(error)

plot(jitter(rez,3) ~ jitter(test_labels, 3), pch = 3, xlab='actual', ylab='predicted')



# all tens
error2 = test_labels - rep(10, length(test_labels))
sum(error2 == 0 )/length(error2)
sum( abs(error2) < 3 )/length(error2)



