#Curso de R para Finanças Quantitativas
#Aula 11 - Introducao a Quantmod library
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)
getSymbols("GOOG", src = "yahoo")

#Periodo de Analise
startdate <- as.Date("2017-01-01")
enddate <- as.Date("2018-09-17")

#Selecao dos ativos para analise
tickers <- c("^BVSP","BTCUSD=X","PETR4.SA")

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)

#Graficos
chartSeries(BVSP)
chartSeries(BVSP, multi.col = T, theme = "white")

#Passar o grafico para o mensal
chartSeries(to.monthly(BVSP), up.col = "green", dn.col = "white")

#Indicadores Tecnicos
library(TTR)
chartSeries(PETR4.SA, TA = NULL) #Retirar o volume do grafico
addMACD()
addBBands()
addCCI()
