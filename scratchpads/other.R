# simple tree
detach("package:randomForest", unload=TRUE)
require(rpart)
dim(d)[1]
s <- d[sample(nrow(d), 6000),]
dim (s)
fit_tree <- rpart(score ~ ., s)
fit_tree <- rpart(score ~ ., d, method='class')
plot(fit_tree)
text(fit_tree)

