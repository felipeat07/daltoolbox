iris <- datasets::iris
head(iris)

#extracting the levels for the dataset
slevels <- levels(iris$Species)
slevels

# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, iris)
iris_train <- sr$train
iris_test <- sr$test

tbl <- rbind(table(iris[,"Species"]),
             table(iris_train[,"Species"]),
             table(iris_test[,"Species"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

model <- cla_knn("Species", slevels, k=1)
print(describe(model))
model <- fit(model, iris_train)
train_prediction <- predict(model, iris_train)

iris_train_predictand <- adjustClassLabels(iris_train[,"Species"])
train_eval <- evaluate(model, iris_train_predictand, train_prediction)
print(train_eval$metrics)

# Test
test_prediction <- predict(model, iris_test)

iris_test_predictand <- adjustClassLabels(iris_test[,"Species"])
test_eval <- evaluate(model, iris_test_predictand, test_prediction)
print(test_eval$metrics)





#  Gráficos em séries temporais
#  Harbinger retestar
#  Repositorio DAL Extended ToolBox
#  Documentação do DAL
