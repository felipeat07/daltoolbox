i <- seq(0, 25, 0.25)
x <- cos(i)

plot(i, x)
lines(i, x)

sw_size <- 10
ts <- ts_data(x, sw_size)
tshead(ts, 3)

test_size <- 1
samp <- ts_sample(ts, test_size)
tshead(samp$train, 3)
tshead(samp$test)

tune <- ts_tune(input_size=c(3:5), base_model = ts_elm(preprocess = ts_gminmax()))
ranges <- list(nhid = 1:5, actfun=c('sig', 'radbas', 'tribas', 'relu', 'purelin'))

io_train <- ts_projection(samp$train)
model <- fit(tune, x=io_train$input, y=io_train$output, ranges)

print(describe(model))

adjust <- predict(model, io_train$input)
ev_adjust <- evaluate(model, io_train$output, adjust)
print(head(ev_adjust$metrics))

steps_ahead <- 1
io_test <- ts_projection(samp$test)
prediction <- predict(model, x=io_test$input, steps_ahead=steps_ahead)
prediction <- as.vector(prediction)

output <- as.vector(io_test$output)
if (steps_ahead > 1)
  output <- output[1:steps_ahead]

print(sprintf("%.2f, %.2f", output, prediction))

ev_test <- evaluate(model, output, prediction)
print(head(ev_test$metrics))
print(sprintf("%s: smape: %.2f", describe(model), 100*ev_test$metrics$smape))

yvalues <- c(io_train$output, io_test$output)
tsplot(model, y=yvalues, yadj=adjust, ypre=prediction)
