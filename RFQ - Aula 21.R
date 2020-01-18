#Curso de R para Finanças Quantitativas
#Aula 21 - Regressao Linear - Parte 6 - Funçao Predict e Implementaçao
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)


#####
#Selecao dos ativos para analise
tickers <- c("^BVSP")

#Periodo de Analise
startdate <- as.Date("2017-01-01")
enddate <- as.Date("2019-01-11")

getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)
BVSP <- data.frame(BVSP)
BVSP <- na.omit(BVSP)
GOOG <- data.frame(GOOG)
GOOG <- na.omit(GOOG)

#Renomenado o DF
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")
names(GOOG) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")

#####
#Variaveis baseadas na medida do tamanho da minima e volume
BVSP$Min <- abs(BVSP$Open - BVSP$Low)
summary(BVSP$Min)

BVSP$Volume <- BVSP$Volume
BVSP$Rsi <- RSI(BVSP$Close,7,"SMA")
BVSP$Macd <- MACD(BVSP$Close,12,26,9,"SMA") 
BVSP$Macd <- BVSP$Macd[,1] - BVSP$Macd[,2] 
BVSP$Bbands <- BBands(BVSP$Close,20,"SMA",2)
BVSP$Close_Shift <- BVSP$Close
BVSP['Close_Shift'] <- c(NA, head(BVSP['Close_Shift'], dim(BVSP)[1] - 1)[[1]])
BVSP$Close_Shift2 <- BVSP$Close_Shift
BVSP['Close_Shift2'] <- c(NA, head(BVSP['Close_Shift2'], dim(BVSP)[1] - 1)[[1]])
BVSP$Return <- (BVSP$Close/BVSP$Close_Shift-1)*100
BVSP$Return2 <- (BVSP$Close/BVSP$Close_Shift2-1)*100

#Criando o alvo
desloca <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

BVSP$Alvo1 <- desloca(BVSP$Return, 1)
BVSP$Alvo2 <- desloca(BVSP$Return2, 1)
abline(regressao2)


par(mfrow=c(1,2))
plot(BVSP$Alvo1, col = "red", lwd = 2,
     main = "Alvo da Regressao #1",
     xlab = "Janeiro - Dezembro 2018",
     ylab = "Retorno em %")
abline(regressao)

plot(BVSP$Alvo2, col = "green", lwd = 2,
     main = "Alvo da Regressao #2",
     xlab = "Janeiro - Dezembro 2018",
     ylab = "Retorno em %")
abline(regressao2)

BVSP <- subset(BVSP, Volume > 0)
BVSP <- subset(BVSP, Min != 0)

BVSP_train <- BVSP[1:304,]
BVSP_test <- BVSP[305:dim(BVSP)[1],]

#####
#Ajustando a regressao 1 
regressao <- lm(Alvo1 ~ Min + Macd + Bbands[,4] + Rsi + Volume, data = BVSP_train)


#####
#Ajustando a regressao 2
regressao2 <- lm(Alvo2 ~  Min + Macd + Bbands[,4] + Rsi + Volume, data = BVSP_train)
x <- c(1,2,3,4)
y <- c(1,2,3,4)
exists(teste <- lm(y ~  x))
exists("teste")
rm(a)
#####
#Predicao dos resultados na base de teste
BVSP_test$predicao <- predict(regressao,BVSP_test)
BVSP_test$predicao2 <- predict(regressao2,BVSP_test)
BVSP_test <- na.omit(BVSP_test)

BVSP_test$AlvoBIN1 <- ifelse(BVSP_test$Alvo1 > 0,1,0)
BVSP_test$AlvoBIN2 <- ifelse(BVSP_test$Alvo2 > 0,1,0)
BVSP_test$predBIN <- ifelse(BVSP_test$predicao > 0,1,0)
BVSP_test$predBIN2 <- ifelse(BVSP_test$predicao2 > 0,1,0)

round(prop.table(table(BVSP_test$AlvoBIN1,BVSP_test$predBIN),1)*100,2)
round(prop.table(table(BVSP_test$AlvoBIN2,BVSP_test$predBIN2),1)*100,2)

retorno_modelo <- ifelse(BVSP_test$predBIN > 0,BVSP_test$Alvo1, -1*BVSP_test$Alvo1)
retorno_modelo_acumulado <- cumsum(retorno_modelo)

retorno_modelo2 <- ifelse(BVSP_test$predBIN2 > 0,BVSP_test$Alvo2, -1*BVSP_test$Alvo2)
retorno_modelo_acumulado2 <- cumsum(retorno_modelo2)

#####
#Grafico das regressoes
par(mfrow=c(1,2))
plot(retorno_modelo_acumulado, type = "l", col = "red", lwd = 2,
     main = "Retorno da Regressao #1",
     xlab = "Agosto 2018 - Janeiro 2019",
     ylab = "Retorno em %")


plot(retorno_modelo_acumulado2, type = "l", col = "green", lwd = 2,
     main = "Retorno da Regressao #2",
     xlab = "Agosto 2018 - Janeiro 2019",
     ylab = "Retorno em %")
par(mfrow=c(1,1))

#####
#Trabalhando a funçao predict
#Intervalo de Previsao
regressao2$coefficients

previsto <- data.frame(predict(regressao2,BVSP_test,interval='prediction'))
rownames(previsto) <- c()


plot(previsto$fit, 
     type = "p", 
     lwd = 2 ,
     main = "Valores Previstos e Intervalos Inf. e Sup. da Previsao",
     ylim = c(min(previsto$lwr), max(previsto$upr)+2),
     ylab = "Previsto")
lines(previsto$lwr, type = "l",lwd = 2 , col = "red")
lines(previsto$upr, type = "l",lwd = 2 ,col = "blue")


for (i in seq(1,dim(previsto)[1])) {
  rect(i, previsto$lwr[i], i, previsto$upr[i],lty = 2)
} 

#Legenda
legend(1, 6.5, legend=c("Previsto", "Banda Inf.", "Banda Sup."),
       col=c("black","red", "blue"), lty=1:1, cex=0.8,horiz=TRUE)
text(70,6,"Outspoken Market")

#Teste se o valor real fica entre as bandas
lines(BVSP_test$Alvo2, type = "l",lwd = 2 ,col = "purple")
