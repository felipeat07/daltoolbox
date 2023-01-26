# DAL Library
# version 2.1

library(elmNNRcpp)

i <- seq(0, 25, 0.25)
x <- cos(i)

plot(i, x)
lines(i, x)

sw_size <- 10
ts <- ts_data(x, sw_size)
head(ts, 3)

test_size <- 1
samp <- ts_sample(ts, test_size)
head(samp$train, 3)
head(samp$test)

tune <- ts_maintune(preprocess=list(ts_gminmax()), input_size=c(3:5), base_model = ts_elm(), augment = list(ts_augment()))
ranges <- list(nhid = 1:5, actfun=c('sig', 'radbas', 'tribas', 'relu', 'purelin'))

io_train <- ts_projection(samp$train)
model <- fit(tune, x=io_train$input, y=io_train$output, ranges)

print(describe(model))

adjust <- predict(model, io_train$input)
ev_adjust <- evaluation.tsreg(io_train$output, adjust)
print(head(ev_adjust$metrics))

steps_ahead <- 1
io_test <- ts_projection(samp$test)
prediction <- predict(model, x=io_test$input, steps_ahead=steps_ahead)
prediction <- as.vector(prediction)

output <- as.vector(io_test$output)
if (steps_ahead > 1)
  output <- output[1:steps_ahead]

print(sprintf("%.2f, %.2f", output, prediction))

ev_test <- evaluation.tsreg(output, prediction)
print(head(ev_test$metrics))
print(sprintf("%s: smape: %.2f", describe(model), 100*ev_test$metrics$smape))

yvalues <- c(io_train$output, io_test$output)
plot(model, y=yvalues, yadj=adjust, ypre=prediction)

### Ranges for ELM
ranges_elm <- list(nhid = 1:20, actfun=c('sig', 'radbas', 'tribas', 'relu', 'purelin'))

### Ranges for MLP
ranges_mlp <- list(size = 1:10, decay = seq(0, 1, 1/9), maxit=10000)

### Ranges for RF
ranges_rf <- list(nodesize=1:10, ntree=1:10)

### Ranges for SVM
ranges_svm <- list(kernel=c("radial", "poly", "linear", "sigmoid"), epsilon=seq(0, 1, 0.1), cost=seq(20, 100, 20))

### Ranges for LSTM
ranges_lstm <- list(epochs=10000)

preprocessing <- list(ts_gminmax(), ts_diff(), ts_swminmax(), ts_an(), ts_ean())

augment <- list(ts_augment(), jitter(), stretch(), shrink(), flip(), wormhole())


