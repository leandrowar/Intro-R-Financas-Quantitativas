#Curso de R para Finanças Quantitativas
#Aula 6 - A funçao tapply
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa",
           "sa", "act", "nsw", "vic", "vic", "act")

statef <- factor(state)
levels(statef)



incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)

#Funçao tapply

incmeans <- tapply(incomes,statef,mean)
incmeans

#Funçao customizada

stderror <- function(x) sqrt(var(x)/length(x))

incstr <- tapply(incomes,statef,stderror)
incstr
