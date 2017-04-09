if (!require('Metrics')) install.packages('Metrics')
library(Metrics)


# split training /training
set.seed(2) 
h=sample(c(0,1),prob=c(0.2,0.8),nrow(sparse_matrix),replace=T)
train=d[h==1,]
test=d[h==0,]

train_labels = d$score[h==1]
test_labels = d$score[h==0]

# random forest
if (!require('randomForest')) install.packages('randomForest')
require(randomForest)
set.seed(42)

fit=randomForest(factor(score)~., data=train)
#saveRDS(fit, 'randomForest_fit.RDS')
print(fit)
varImpPlot(fit)
importance(fit)


# predict
p <- predict(fit, test)
auc(test$score, p)

summary(p)
levels(p)[p]
str(p)
as.integer(p)

plot(jitter(as.integer(p),2) ~ jitter(as.integer(test$score), 2), pch = 3)
