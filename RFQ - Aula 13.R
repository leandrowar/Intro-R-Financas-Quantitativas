#Curso de R para Finanças Quantitativas
#Aula 13 - Explorando os dados e missings
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)

#Periodo de Analise
startdate <- as.Date("2017-01-01")
enddate <- as.Date("2018-09-24")

#Selecao dos ativos para analise
tickers <- c("^BVSP")

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)

BVSP <- data.frame(BVSP)

#Descriçao basica e frequencias
summary(BVSP)

#Editor de dados
edit(BVSP)


#Estrutura do dataframe
str(BVSP)


#Lista de variaveis e mudança de nome
names(BVSP)
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")
names(BVSP)

#Primeiras linhas
head(BVSP)
head(BVSP, n = 10)


#Ultimas Linhas
tail(BVSP)
tail(BVSP, n = 10)

#Seleçao de dados
BVSP[1:10,]
BVSP[15,4] <- 66666
BVSP[15,4]

#Trabalhando com missings
colSums(is.na(BVSP))
BVSP <- na.omit(BVSP)
colSums(is.na(BVSP))
