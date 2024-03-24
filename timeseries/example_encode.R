library(daltoolbox)

data(sin_data)

sin_data$y[39] <- sin_data$y[39]*6
plot(sin_data$x, sin_data$y)

sw_size <- 5
ts <- ts_data(sin_data$y, sw_size)

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)

samp <- ts_sample(ts, test_size = 10)
train <- as.data.frame(samp$train)
test <- as.data.frame(samp$test)


auto <- autoenc_encode(5, 3)

auto <- fit(auto, train)

print(head(test))
result <- transform(auto, test)
print(head(result))
