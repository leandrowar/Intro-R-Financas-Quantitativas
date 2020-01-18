#Curso de R para Finanças Quantitativas
#Aula 16 - Introducao a Regressao Linear
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Dataset
cars

plot(cars$speed, cars$dist,
     pch = 16, cex = 1,
     col = "blue",
     main = "Speed x Distance",
     xlab = "Speed",
     ylab = "Distance")
abline(regressao)

#Regressao Linear
regressao <- lm(cars$dist ~ cars$speed)
regressao
summary(regressao)
