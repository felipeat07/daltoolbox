# DAL Library
# version 2.1

#source("examples/examples.R")

#loading DAL
#load_dal() # see ../examples.R

# needed for ts_arima.R
loadlibrary("elmNNRcpp")

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

preproc <- ts_gminmax()

model <- ts_elm(preproc, input_size=4, nhid=3, actfun="purelin")

io_train <- ts_projection(samp$train)
model <- fit(model, x=io_train$input, y=io_train$output)

print(describe(model))

adjust <- predict(model, io_train$input)
adjust <- as.vector(adjust)
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

yvalues <- c(as.vector(io_train$output), as.vector(io_test$output))
length(yvalues)

plot.tsreg(model, y=yvalues, yadj=adjust, ypre=prediction)

length(c(adjust, prediction))


