#Curso de R para Finanças Quantitativas
#Aula 14 - Frequencias e Tabelas Cruzadas Parte 1
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

#Renomenado o DF
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")


#Medida do tamanho do candle
BVSP$Corpo <- BVSP$Close -BVSP$Open
summary(BVSP$Corpo)
BVSP$CorpoCategorias <- ifelse(BVSP$Corpo > 600 , "C3Q", "<C3Q")

summary(BVSP$Volume)
BVSP$VolumeCategorias <- ifelse(BVSP$Volume > 3742250 , "V3Q", "<V3Q")

table(BVSP$VolumeCategorias,BVSP$CorpoCategorias)

tabela <- table(BVSP$VolumeCategorias,BVSP$CorpoCategorias)

prop.table(tabela) #Proporçao total da base
prop.table(tabela,1) #Proporçao por linha
prop.table(tabela,2) #Proporçao por coluna

round(prop.table(tabela,2)*100,2)

library(gmodels)
CrossTable(BVSP$VolumeCategorias,BVSP$CorpoCategorias)
CrossTable(BVSP$VolumeCategorias,BVSP$CorpoCategorias)
