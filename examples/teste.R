i <- seq(0, 25, 0.25)
x <- cos(i)

#library(ggplot2)
serie <- data.frame(x=i, cos=x)
grf <- plot_series(serie, colors="black")
font <- theme(text = element_text(size=16))
#plot(grf+font)

sw_size <- 0
ts <- ts_data(x, 0)
tshead(ts, 3)

test_size <- 1
samp <- ts_sample(ts, test_size)
tshead(samp$train, 3)
tshead(samp$test)

model <- tsreg_arima()

io_train <- ts_projection(samp$train)
model <- fit(model, x=io_train$input, y=io_train$output)

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


#grf <- mytsplot(model, y=yvalues, yadj=adjust, ypre=prediction)
#grf <- ts_plot(y = yvalues)
#plot(grf)

grf <- ts_plot_pred(y = yvalues, yadj = adjust, ypred = prediction)
plot(grf)

#  Gráficos em séries temporais
#  Harbinger retestar
#  Repositorio DAL Extended ToolBox
#  Documentação do DAL
