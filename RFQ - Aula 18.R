#Curso de R para Finanças Quantitativas
#Aula 18 - Regressao Linear - Parte 3 - Bases de Treinamento e Teste e o MAPE
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra
library(data.table)
library(ggplot2)
library(reshape2)
library(corrplot)

#####
#Carregando o Dataset
songs <- fread("YearPredictionMSD.txt", header = FALSE, sep = ",")
songs <- data.frame(songs)
names(songs)

#####
#train: first 463,715 examples 
#test: last 51,630 examples 
songs_train <- songs[1:463715,]
songs_test <- songs[463716:dim(songs)[1],]

#####
#Ajustando a regressao
regressao <- lm(V1 ~ ., data = songs_train)
sumario <- summary(regressao)
r.squared <- sumario$r.squared
fstatistic <- sumario$fstatistic

#####
#Selecionando apenas as variaveis com significancia menor que 1%
melhores_vars <- sumario$coeff[-1,4] < 0.001
melhores_vars <- names(melhores_vars)[melhores_vars == TRUE] 
#Nao se esqueca de adicionar o alvo
melhores_vars <- c("V1",melhores_vars)
songs_train_melhores <- songs_train[melhores_vars] 

#####
#Ajustando a regressao2
regressao2 <- lm(V1 ~ ., data = songs_train_melhores)
sumario2 <- summary(regressao2)
r.squared2 <- sumario2$r.squared
fstatistic2 <- sumario2$fstatistic

r.squared 
r.squared2
fstatistic
fstatistic2

#####
#Predicao dos resultados na base de trainamento
songs_train_melhores$predicao <- predict(regressao2,songs_train_melhores)
songs_train_melhores$predicao <- as.integer(songs_train_melhores$predicao)

#####
#Calculando o Mean absolute percentage error - MAPE - na base de treinamento
MAPE<-function(actual,predicted){
  (mean(abs((actual-predicted)/predicted)))*100
  }

mape_train <- MAPE(songs_train_melhores$V1,songs_train_melhores$predicao)
mape_train

#####
#Predicao dos resultados na base de teste
songs_test$predicao <- predict(regressao2,songs_test)
songs_test$predicao <- as.integer(songs_test$predicao)

#####
#Calculando o Mean absolute percentage error - MAPE - na base de teste
mape_test <- MAPE(songs_test$V1,songs_test$predicao)
mape_test

