#Pacotes ou libraries
#data.table
#install.packages("data.table")
library(data.table)
library("reshape2")
library("ggplot2")

#Carregando um arquivo
euro <- fread("EURUSD_H1.csv", header = TRUE, sep = ";")
euro <- as.data.frame(euro)
euro$Date <- as.Date(euro$Date,"%d/%m/%Y")
#euro$MA <-rollapply(euro$Retorno, width = 15, FUN = mean, fill = NA)
#euro$SD <-rollapply(euro$Retorno, width = 15, FUN = sd, fill = NA)
euro$simul  <- rollapply(euro$Retorno, width = 15, 
                        function(x) rnorm(x, mean = (x),sd = (x)), 
                        fill = NA)[,1] 

euro$simul <- ifelse(is.nan(euro$simul) | is.na(euro$simul),0,euro$simul)
euro$simul_Shift <- euro$simul 
euro['simul_Shift'] <- c(NA, head(euro['simul_Shift'], dim(euro)[1] - 1)[[1]])

euro$real_ret <- exp(cumsum(euro$Retorno))-1
euro$sim_ret <- exp(cumsum(euro$simul))-1

euro$trade <- ifelse(euro$simul_Shift>0,euro$Retorno,-1*euro$Retorno)
euro$trade <- ifelse(is.nan(euro$trade) | is.na(euro$trade),0,euro$trade)
euro$trade_ret <- exp(cumsum(euro$trade))-1

ggplot(euro, aes(Date)) + 
  geom_line(aes(y = real_ret, colour = "Real")) + 
  geom_line(aes(y = trade_ret, colour = "Trade"))

