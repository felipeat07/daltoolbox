# DAL Library
# version 2.1

source("examples/examples.R")

#loading DAL
load_dal() # see ../examples.R

i <- seq(0, 2*pi+8*pi/50, pi/50)
x <- cos(i)
noise <- rnorm(length(x), 0, sd(x)/10)

x <- x + noise
x[30] <-rnorm(1, 0, sd(x))

x[60] <-rnorm(1, 0, sd(x))

x[90] <-rnorm(1, 0, sd(x))

plot(i, x)
lines(i, x)

filter <- ts_smooth()
filter <- fit(filter, x)
y <- transform(filter, x)

plot(x = i, y = x, main = "cosine")
lines(x = i, y = x, col="black")
lines(x = i, y = y, col="green")

filter <- ts_ma(3)
filter <- fit(filter, x)
y <- transform(filter, x)

plot(x = i, y = x, main = "cosine")
lines(x = i, y = x, col="black")
lines(x = i, y = y, col="green")

filter <- ts_ema(3)
filter <- fit(filter, x)
y <- transform(filter, x)

plot(x = i, y = x, main = "cosine")
lines(x = i, y = x, col="black")
lines(x = i, y = y, col="green")


