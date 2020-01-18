#Curso de R para Finanças Quantitativas
#Aula 12 - Trading com Quantmod library
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)
#Indicadores Tecnicos
library(TTR)

#Periodo de Analise
startdate <- as.Date("2017-01-01")
enddate <- as.Date("2018-09-19")

#Selecao dos ativos para analise
tickers <- c("^BVSP","BTCUSD=X","PETR4.SA")

#Captura dos dados
getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)

#Graficos
chartSeries(PETR4.SA, TA = NULL) #Retirar o volume do grafico
addMACD()

#Criando a estrategia
macd <- MACD(PETR4.SA$PETR4.SA.Close, nFast = 12, nSlow = 26, nSign = 9, maType = SMA, percent = F)
tradeRule <- Lag(ifelse(macd$macd < macd$signal, -1, 1))

#Calculando os retornos
retornos <- ROC(PETR4.SA$PETR4.SA.Close)*tradeRule
retornos <- retornos["2018-01-01/2018-09-19"]


#Grafico da performance da carteira
carteira <- exp(cumsum(retornos$PETR4.SA.Close))-1
plot(carteira)

#Avaliaçao da Carteira
library(PerformanceAnalytics)
table.Drawdowns(retornos, top = 10)
table.DownsideRisk(retornos)
charts.PerformanceSummary(retornos)
