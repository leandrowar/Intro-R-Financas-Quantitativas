#Curso de R para Finanças Quantitativas
#Aula 10 - O seu primeiro algo-trading
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra


##########################################################################
library(quantmod)

#Periodo de Analise
startDate <- as.Date("2017-01-01")   
endDate <- as.Date("2018-09-12")

#Seleçao das Variaveis
tickers <- c('^BVSP')

#Captura dos dados  
getSymbols(tickers, src = "yahoo", from = startDate, to = endDate)

#Testando com o IBovespa
#Plotando os graficos, escala normal e log
chartSeries(BVSP,TA=NULL)
chartSeries(BVSP,TA=NULL,log.scale=TRUE)
##########################################################################

##### Criando seu primeiro Indicador - A Estratégia da barra anterior #####
#Convertando o objeto xts em dataframe
BVSP <- data.frame(BVSP)

#Ajustando os nomes
names(BVSP) <- c("Open","High","Low","Close","Volume","Adjusted")

##### Começando a montar a estrategia #####

##### Ajustando a maxima e a minima para comparaçao ##### 
BVSP <- as.data.frame(BVSP)

BVSP$High_Shift <- BVSP$High 
BVSP['High_Shift'] <- c(NA, head(BVSP['High_Shift'], dim(BVSP)[1] - 1)[[1]])
BVSP$Low_Shift <- BVSP$Low 
BVSP['Low_Shift'] <- c(NA, head(BVSP['Low_Shift'], dim(BVSP)[1] - 1)[[1]])

##### Ajustando as regras de compra e venda ##### 
BVSP$Bull <- ifelse(BVSP$High > BVSP$High_Shift,(BVSP$Close - BVSP$High_Shift)*0.2,0)
BVSP$Bull[is.na(BVSP$Bull)] <- 0

BVSP$Bear <- ifelse(BVSP$Low < BVSP$Low_Shift,(BVSP$Low_Shift-BVSP$Close)*0.2,0)
BVSP$Bear[is.na(BVSP$Bear)] <- 0

##### Somando os resultados ##### 
BVSP_Results <- cumsum(BVSP$Bull+BVSP$Bear) 

##### Plotando o grafico de performance ##### 
plot(index(BVSP_Results),coredata(BVSP_Results),  
     type = 'l',  
     col="blue",  
     main="IBovespa - Criando seu primeiro Indicador - A Estratégia da barra anterior",
     xlab = "# Dias em 207 e 2018",
     ylab = "Retorno em Reais"
)
text(175,2000,paste("Retorno total ->","R$",tail(BVSP_Results,1)," "))
text(175,1200,"www.outspokenmarket.com")
text(175,500,"fb.com/outspokenmarket")
