#Curso de R para Finanças Quantitativas
#Aula 19 - Regressao Linear - Parte 4 - Diagnostico da Regressao
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)


#####
#Selecao dos ativos para analise
tickers <- c("^BVSP")

#Periodo de Analise
startdate <- as.Date("2017-01-01")
enddate <- as.Date("2018-12-03")

getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)
BVSP <- data.frame(BVSP)
BVSP <- na.omit(BVSP)

#Renomenado o DF
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")


#####
#Variaveis baseadas na medida do tamanho da minima e volume
BVSP$Min <- abs(BVSP$Open - BVSP$Low)
summary(BVSP$Min)

BVSP$Volume <- BVSP$Volume
BVSP$Rsi <- RSI(BVSP$Close,14,"SMA")
BVSP$Macd <- MACD(BVSP$Close,12,26,9,"SMA") 
BVSP$Macd <- BVSP$Macd[,1] - BVSP$Macd[,2] 
BVSP$Bbands <- BBands(BVSP$Close,20,"SMA",2)

BVSP$Close_Shift <- BVSP$Close
BVSP['Close_Shift'] <- c(NA, head(BVSP['Close_Shift'], dim(BVSP)[1] - 1)[[1]])
BVSP$Return <- (BVSP$Close/BVSP$Close_Shift-1)*100

#Criando o alvo
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

BVSP$Alvo <- desloca(BVSP$Return, 1)

BVSP <- subset(BVSP, Volume > 0)
BVSP <- subset(BVSP, Min != 0)

BVSP_train <- BVSP[1:300,]
BVSP_test <- BVSP[301:dim(BVSP)[1],]

#####
#Ajustando a regressao 1 
regressao <- lm(Alvo ~ Min + Macd + Bbands[,4] + Rsi + Volume, data = BVSP_train)
sumario <- summary(regressao)
r.squared <- sumario$r.squared
fstatistic <- sumario$fstatistic
sumario

par(mfrow=c(2,2))
plot(regressao)

#####
#Ajustando a regressao 2
regressao2 <- lm(Alvo ~  Macd + Bbands[,4] + Rsi, data = BVSP_train)
sumario2 <- summary(regressao2)
r.squared2 <- sumario2$r.squared
fstatistic2 <- sumario2$fstatistic
sumario2

plot(regressao2)


#####
#Predicao dos resultados na base de teste
BVSP_test$predicao <- predict(regressao,BVSP_test)
BVSP_test$predicao2 <- predict(regressao2,BVSP_test)
BVSP_test <- na.omit(BVSP_test)

BVSP_test$AlvoBIN <- ifelse(BVSP_test$Alvo > 0,1,0)
BVSP_test$predBIN <- ifelse(BVSP_test$predicao > 0,1,0)
BVSP_test$predBIN2 <- ifelse(BVSP_test$predicao2 > 0,1,0)

round(prop.table(table(BVSP_test$AlvoBIN,BVSP_test$predBIN),1)*100,2)
round(prop.table(table(BVSP_test$AlvoBIN,BVSP_test$predBIN2),1)*100,2)

retorno_modelo <- ifelse(BVSP_test$predBIN > 0,BVSP_test$Alvo, -1*BVSP_test$Alvo)
retorno_modelo_acumulado <- cumsum(retorno_modelo)

retorno_modelo2 <- ifelse(BVSP_test$predBIN2 > 0,BVSP_test$Alvo, -1*BVSP_test$Alvo)
retorno_modelo_acumulado2 <- cumsum(retorno_modelo2)

#####
#Grafico das regressoes
par(mfrow=c(1,2))
plot(retorno_modelo_acumulado, type = "l", col = "red", lwd = 2,
     main = "Retorno da Regressao #1",
     xlab = "Março - Novembro 2018",
     ylab = "Retorno em %")


plot(retorno_modelo_acumulado2, type = "l", col = "green", lwd = 2,
     main = "Retorno da Regressao #2",
     xlab = "Março - Novembro 2018",
     ylab = "Retorno em %")
