if (!require('Metrics')) install.packages('Metrics')
library(Metrics)


# split training /training
set.seed(2) 

d_reduced = d[, c(3,5)]
names(d_reduced) = c('transactions','first_booking')
str(d_reduced)
d_reduced_m = sparse.model.matrix(~., data = d_reduced)

h=sample(c(0,1),prob=c(0.2,0.8),nrow(d_reduced_m),replace=T)
train=d_reduced_m[h==1,]
test=d_reduced_m[h==0,]

train_labels = d$score[h==1]
test_labels = d$score[h==0]

bst <- xgboost(data = train, label = train_labels, max.depth = 13,
               eta = 1, nthread = 1, nround = 200,objective = "multi:softmax", num_class = 11)

importance <- xgb.importance(feature_names = d_reduced_m@Dimnames[[2]], model = bst)
head(importance)

# predict
p <- predict(bst, test)

summary(as.factor(p))
auc(test_labels, p)

rez <- p

error = test_labels - rez
sum(error == 0 )/length(error)
sum( abs(error) < 3 )/length(error)

plot(jitter(rez,3) ~ jitter(test_labels, 3), pch = 3)



# all tens
error2 = test_labels - rep(10, length(test_labels))
sum(error2 == 0 )/length(error2)
sum( abs(error2) < 3 )/length(error2)



