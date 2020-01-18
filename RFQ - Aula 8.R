#Curso de R para Finanças Quantitativas
#Aula 8 - Funçao Summary e Histogramas
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Pacotes ou libraries
#data.table
#install.packages("data.table")
library(data.table)

#Carregando um arquivo
euro <- fread("EURUSD_H1.csv", header = TRUE, sep = ";")
class(euro)

#Convertendo para Dataframe
euro <- as.data.frame(euro)

#Funçao summary
summary(euro)


#O histograma dos fechamentos
hist(euro$Close)


#Melhorando a visualizaçao dos histogramas
hist(euro$Close, seq(1.0, 1.3, 0.05))
hist(euro$Close, seq(1.0, 1.3, 0.005))
