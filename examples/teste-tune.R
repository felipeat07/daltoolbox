library(MASS)
data(Boston)
print(t(sapply(Boston, class)))
head(Boston)

# for performance issues, you can use matrix
Boston <- as.matrix(Boston)

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, Boston)
boston_train = sr$train
boston_test = sr$test

#tune <- reg_tune(reg_knn("medv", k=3))
#ranges <- list(k=1:10)
#tune <- reg_tune(reg_mlp("medv", size=5, decay=0.54))
#ranges <- list(size=1:5, decay=seq(0, 1, 0.1))
#tune <- reg_tune(reg_rf("medv", mtry=7,ntree=30))
#ranges <- list(mtry=1:10, ntree=1:10)
tune <- reg_tune(reg_svm("medv"))
ranges <- list(seq(0,1,0.2), cost=seq(20,100,20), kernel = c("linear", "radial", "polynomial", "sigmoid"))
model <- fit(tune, boston_train, ranges)

train_prediction <- predict(model, boston_train)
boston_train_predictand <- boston_train[,"medv"]
test_eval <- evaluate(model, boston_train_predictand, train_prediction)
print(test_eval$metrics)


test_prediction <- predict(model, boston_test)
boston_test_predictand <- boston_test[,"medv"]
test_eval <- evaluate(model, boston_test_predictand, test_prediction)
print(test_eval$metrics)
