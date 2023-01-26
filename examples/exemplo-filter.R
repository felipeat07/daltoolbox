i <- seq(0, 2*pi+8*pi/50, pi/50)
x <- cos(i)
noise <- rnorm(length(x), 0, sd(x)/10)

x <- x + noise
x[30] <-rnorm(1, 0, sd(x))

x[60] <-rnorm(1, 0, sd(x))

x[90] <-rnorm(1, 0, sd(x))

plot(i, x)
lines(i, x)

myfilter <- ts_smooth()
myfilter <- fit(myfilter, x)
y <- transform(myfilter, x)
