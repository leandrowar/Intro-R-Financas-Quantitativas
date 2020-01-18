#Curso de R para Finanças Quantitativas
#Aula 17 - Regressao Linear - Parte 2
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra
library(data.table)
library(ggplot2)
library(reshape2)
library(corrplot)

#####
#Carregando o Dataset
portfolio <- fread("stock_portfoglio_performance.csv", header = TRUE, sep = ";")
portfolio <- data.frame(portfolio)
names(portfolio)


#####
#Escolhendo as variaveis para manter
manter <- c("BP","ROE","SP","Return_last_quarter","Market_Value", "Small_systematic_Risk","Rel_Win_Rate")
portfolio <- portfolio[ , (names(portfolio) %in% manter)]
names(portfolio)

#####
#Plotando o grafico de cada variavel em relaçao ao alvo
portfolio2 <- melt(portfolio, id.vars='Rel_Win_Rate')
ggplot(portfolio2) +
  geom_jitter(aes(value,Rel_Win_Rate, colour=variable)) + geom_smooth(aes(value,Rel_Win_Rate, colour=variable), method=lm, se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Valores", y = "Rel_Win_Rate")

#####
#Ajustando a regressao
regressao <- lm(Rel_Win_Rate ~ ., data = portfolio)
regressao
summary(regressao)

#Remover variaveis
manter <- c("BP","ROE","Return_last_quarter", "Small_systematic_Risk","Rel_Win_Rate")
portfolio <- portfolio[ , (names(portfolio) %in% manter)]
names(portfolio)

#Ajustando a regressao2
regressao2 <- lm(Rel_Win_Rate ~ ., data = portfolio)
regressao2
summary(regressao2)

#Remover variaveis
manter <- c("ROE","Return_last_quarter", "Small_systematic_Risk","Rel_Win_Rate")
portfolio <- portfolio[ , (names(portfolio) %in% manter)]
names(portfolio)

#Ajustando a regressao3
regressao3 <- lm(Rel_Win_Rate ~ ROE + Return_last_quarter + Small_systematic_Risk, data = portfolio)
summary(regressao3)
plot(regressao3)

