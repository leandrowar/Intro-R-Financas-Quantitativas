#Curso de R para Finanças Quantitativas
#Aula 4 - Ler arquivos e Data Frames
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Pacotes ou libraries
#data.table
#install.packages("data.table")
library(data.table)

#Carregando um arquivo
x <- c(1,2)
euro <- fread("EURUSD_H1.csv", header = TRUE, sep = ";")
class(euro)

#Convertendo para Dataframe
euro <- as.data.frame(euro)
class(euro)


#Conhecendo sua base de dados
dim(euro)
names(euro)

#Acessando o dataset
names(euro)[1]
euro[1]
euro[1,1]
euro[1,2]
euro[100,2]
