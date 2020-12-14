#Curso de R para Finanças Quantitativas
#Aula 23 - Regressao Linear - Parte 8 - Mercados eficientes?!
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

library(quantmod)
library(caTools)
library(tidyverse)
library(lubridate)

#Instagram: @leandrowar


#####
#Selecao dos ativos para analise
tickers <- c("^BVSP")

#Periodo de Analise
startdate <- as.Date("2015-01-01")
enddate <- as.Date("2020-12-07")

getSymbols(tickers, src = "yahoo", from = startdate, to = enddate)
BVSP <- data.frame(BVSP)
BVSP <- na.omit(BVSP)

#Renomenado o DF
names(BVSP) <- c("Open", "High", "Low", "Close", "Volume", "Ajustado")

rownames(BVSP)

BVSP$Data <- rownames(BVSP)

BVSP$Data <- parse_date_time(BVSP$Data, "ymd")

#####

# Calculando os retornos e o alvo
BVSP <- mutate(BVSP, Close_lead = lead(Close))
BVSP <- mutate(BVSP, Close_lag = lag(Close)) # vai criar um NA na primeira linha

BVSP <- na.omit(BVSP)

BVSP$Return <- (BVSP$Close/BVSP$Close_lag-1)*100
BVSP$Return_Bin <- ifelse(BVSP$Return > 0, 1,0)
BVSP$Alvo1 <- (BVSP$Close_lead/BVSP$Close-1)*100
BVSP$Alvo1_Bin <- ifelse(BVSP$Alvo1 > 0, 1,0)

#####
# Calculando a média movel dos retornos e o desvio padrao

periodo_ma <- 15

BVSP$MA_Close <- c(rep(NA, periodo_ma-1),runmean(BVSP$Close, periodo_ma, alg=c("C"), endrule=c("trim")))

BVSP$MA_Return <- c(rep(NA, periodo_ma-1),runmean(BVSP$Return, periodo_ma, alg=c("C"), endrule=c("trim")))

BVSP$SD_Return <- c(rep(NA, periodo_ma-1),runsd(BVSP$Return, periodo_ma, endrule=c("trim")))

BVSP <- na.omit(BVSP)

# Calculando a proporçao de dias entre alta e baixa

sum(table(BVSP$Return_Bin))
prop_return <- as.vector(round(100*table(BVSP$Return_Bin)/sum(table(BVSP$Return_Bin)), 1))

par(mfrow=c(1,2))

pie(as.vector(table(BVSP$Return_Bin))
    , prop_return
    , main = "Proporção de dias em baixa e alta \n Ibovespa - 2015-2020"
    , col = c("red", "darkblue")
)
legend("bottomright", c("Baixa", "Alta"), cex = 0.8, fill = c("red", "darkblue"))


# Calculando se o preço esta acima ou abaixo da média mòvel
BVSP$MA_REF <- ifelse(BVSP$Close >= BVSP$MA_Close, 1, 0)

prop_ma_ref <- as.vector(round(100*table(BVSP$MA_REF)/sum(table(BVSP$MA_REF)), 1))


pie(as.vector(table(BVSP$MA_REF))
    , prop_ma_ref
    , main = "Permanência do Ibovespa \n acima da média móvel de 15 dias - 2015-2020"
    , col = c("red", "darkblue")
)
legend("bottomright", c("Abaixo", "Acima"), cex = 0.8, fill = c("red", "darkblue"))


# Calculando a proporçao de altas ou baixas quando està abaixo ou acima da média mòvel
table(BVSP$Return_Bin , BVSP$MA_REF)

prop_ret_abaixo_ma <- as.vector(round(100*table(BVSP$Return_Bin , BVSP$MA_REF)[,1]/sum(table(BVSP$Return_Bin , BVSP$MA_REF)[,1]), 1))
prop_ret_acima_ma <- as.vector(round(100*table(BVSP$Return_Bin , BVSP$MA_REF)[,2]/sum(table(BVSP$Return_Bin , BVSP$MA_REF)[,2]), 1))


# Grafico das proporçoes abaixo da MA
pie(as.vector(table(BVSP$Return_Bin , BVSP$MA_REF)[,1])
    , prop_ret_abaixo_ma
    , main = "Proporção de altas e baixas \n abaixo da média móvel de 15 dias - 2015-2020"
    , col = c("red", "darkblue")
)
legend("bottomright", c("Baixa", "Alta"), cex = 0.8, fill = c("red", "darkblue"))

# Grafico das proporçoes acima da MA
pie(as.vector(table(BVSP$Return_Bin , BVSP$MA_REF)[,2])
    , prop_ret_acima_ma
    , main = "Proporção de altas e baixas \n acima da média móvel de 15 dias - 2015-2020"
    , col = c("red", "darkblue")
)
#legend("bottomright", c("Baixa", "Alta"), cex = 0.8, fill = c("red", "darkblue"))

as.vector(aggregate(Return ~ MA_REF, data = BVSP, FUN = mean)[2])[,1]

par(mfrow=c(1,1))
barplot(as.vector(aggregate(Return ~ MA_REF, data = BVSP, FUN = mean)[2][,1])
        #, col = "blue"
        , main = "Média dos retornos quando abaixo ou acima da média de 15 dias \n Ibovespa - 2015-2020"
        , ylim = c(-0.7, 0.7)
        , xlab = "Abaixo - Acima"
        , ylab = "Retorno (%)"
        , col = c("red","darkblue")
        )
text(x=0.75, y=-0.6, labels="-0.57%")
text(x=1.9, y=0.57, labels="0.53%")

as.vector(aggregate(Alvo1 ~ MA_REF, data = BVSP, FUN = mean)[2])[,1]

par(mfrow=c(1,1))
barplot(as.vector(aggregate(Alvo1 ~ MA_REF, data = BVSP, FUN = mean)[2][,1])
        #, col = "blue"
        , main = "Média dos retornos do dia seguind quando \n abaixo ou acima da média de 15 dias Ibovespa - 2015-2020"
        , ylim = c(0, 0.13)
        , xlab = "Abaixo - Acima"
        , ylab = "Retorno (%)"
        , col = c("darkblue","blue")
)
text(x=0.75, y=0.10, labels="0.09%")
text(x=1.9, y=0.063, labels="0.06%")

data_corte <- "2018-12-31"
BVSP_train <- subset(BVSP, BVSP$Data <= data_corte)
BVSP_test <- subset(BVSP, BVSP$Data > data_corte)

#####
#Ajustando a regressao 1 
regressao <- lm(Alvo1_Bin ~ 
                  Return + MA_REF, data = BVSP_train)
summary(regressao)

#####
#Predicao dos resultados na base de teste
BVSP_test$predicao <- predict(regressao, BVSP_test)

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

plot(BVSP_test$Data,retorno_modelo_acumulado,  type = "l", col = "blue", lwd = 3,
     main = "Retorno da Aula 23 - RFQ - Outspoken Market",
     xlab = "2019 - 2020",
     ylab = "Retorno em %")

par(mfrow=c(2,1))
plot(BVSP_test$Data,retorno_modelo_acumulado,  type = "l", col = "darkgreen", lwd = 3,
     main = "Retorno da Aula 23 - RFQ - Outspoken Market",
     xlab = "2019 - 2020",
     ylab = "Retorno em %")
plot(BVSP_test$Data,BVSP_test$Close,  type = "l", col = "blue", lwd = 3,
     main = "Fechamento Ibovespa Aula 23 - RFQ - Outspoken Market",
     xlab = "2019 - 2020",
     ylab = "Pontos")
lines(BVSP_test$Data, BVSP_test$MA_Close, col = "black", lwd = 2, type = "l")

