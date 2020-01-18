#Curso de R para Finanças Quantitativas
#Aula 5 - Tipos de dados e conversoes
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Vetores e os dataframes
#mesmo tipo de dados
#lista
#Tipos de dados: numeric, logical, complex, character e factor

z <- 0:9
class(z)

d <- as.character(z)
class(d)

digitos <- as.integer(d)
class(digitos)

fatores <- as.factor(digitos)
class(fatores)
