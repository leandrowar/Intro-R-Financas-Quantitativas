#Curso de R para Finanças Quantitativas
#Aula 22 - Regressao Linear - Parte 7 - Novo Modelo Linear com novas variaveis
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)

#@leandrowar


#####
#Selecao dos ativos para analise
tickers <- c("^BVSP")

#Periodo de Analise
startdate <- as.Date("2017-01-01")
enddate <- as.Date("2020-07-21")

getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)
BVSP <- data.frame(BVSP)
BVSP <- na.omit(BVSP)

#Renomenado o DF
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")

#####
#Variaveis baseadas na medida do tamanho da minima e volume
BVSP$Min <- BVSP$Open/BVSP$Low-1
BVSP$Max <- BVSP$High/BVSP$Open-1
BVSP$Candle <- BVSP$High/BVSP$Min-1
summary(BVSP$Candle)

BVSP$Volume <- BVSP$Volume
BVSP$Rsi <- RSI(BVSP$Close,7,"SMA")
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

BVSP$Alvo1 <- desloca(BVSP$Return, 1)
BVSP$Alvo_Class <- BVSP$Alvo1
cutoff <- 0.5
hist(BVSP$Alvo1)
BVSP$Alvo_Class <- ifelse((BVSP$Alvo_Class >= -cutoff & BVSP$Alvo_Class <= cutoff), 0
                          , ifelse(BVSP$Alvo_Class < cutoff, -1, 1))

BVSP <- subset(BVSP, Volume > 0)
BVSP <- subset(BVSP, Min != 0)

BVSP_train <- BVSP[1:304,]
BVSP_test <- BVSP[305:dim(BVSP)[1],]


par(mfrow=c(1,1))
plot(BVSP$Alvo1, col = "red", lwd = 2,
     main = "Alvo da Regressao",
     xlab = "31.07.2018 - 16.07.2020",
     ylab = "Retorno em %")

barplot(table(BVSP$Alvo_Class),
        col = "blue",
        main = "Alvo em Classes",
        xlab = "Classe",
        ylab = "Quantidade de dias")


#####
#Ajustando a regressao 1 
regressao <- lm(Alvo_Class ~ 
                  Return + Min + Max + Candle + Macd + Bbands[,4] 
                + Rsi + Volume, data = BVSP_train)
summary(regressao)

#####
#Predicao dos resultados na base de teste
BVSP_test$predicao <- predict(regressao, BVSP_test)
boxplot(BVSP_test$predicao, main = "Boxplot valores previstos - Aula 22",
        xlab = "Regressao",
        ylab = "Valor Previsto")
summary(BVSP_test$predicao)
summary(BVSP_test$predicao)[3]

BVSP_test$AlvoBIN1 <- ifelse(BVSP_test$Alvo1 > 0, 1, 0)
BVSP_test$predBIN <- ifelse(BVSP_test$predicao > summary(BVSP_test$predicao)[4]
                            , 1, 0)

round(prop.table(table(BVSP_test$AlvoBIN1,BVSP_test$predBIN),1)*100,2)

retorno_modelo <- ifelse(BVSP_test$predBIN > 0,BVSP_test$Alvo1, -1*BVSP_test$Alvo1)

#Simulando um stop de -3%
stop = -3
retorno_modelo <- ifelse(retorno_modelo < stop, stop, retorno_modelo)
retorno_modelo_acumulado <- cumsum(retorno_modelo)


#####
#Grafico do resultado da regressao
par(mfrow=c(1,1))

plot(retorno_modelo_acumulado, type = "l", col = "blue", lwd = 3,
     main = "Retorno da Aula 22 - RFQ - Outspoken Market",
     xlab = "31.07.2018 - 20.07.2020",
     ylab = "Retorno em %")



