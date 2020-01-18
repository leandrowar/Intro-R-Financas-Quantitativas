#Curso de R para Finanças Quantitativas
#Aula 9 - Funçao Summary e Histogramas
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Pacotes ou libraries
#data.table
#install.packages("data.table")
library(data.table)

#Carregando um arquivo
euro <- fread("EURUSD_H1.csv", header = TRUE, sep = ";")
class(euro)

#Plotando o grafico
plot(euro$Close)
plot(euro$Close, col = "blue", type = "l")
plot(euro$Close, col = "blue", type = "l",
     main = "Grafico EUR/USD - H1",
     xlab = "Tempo",
     ylab = "Preço")

x <- 1:nrow(euro)
abline(lm(euro$Close ~ x), col = "red")
abline(h = mean(euro$Close), col = "black")
summary(euro$Close)
