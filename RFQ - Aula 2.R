#Curso de R para Finanças Quantitativas
#Aula 2 - Sequencias e Vetores Logicos
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Sequencia

seq1 <- seq(-10,10, by = 0.5)
seq1

seq2 <- seq(length = 10, from = 0, by =0.1)
seq2

seq3 <- rep(1, times = 1000)
seq3

#Vetores Logicos
#TRUE ou FALSE

v1 <- TRUE
v1

v2 <- 23 > 50
v2

#Operadores Logicos
# < <= > >= = !=
v3 <- v1 != v2
v3

v4 <- v3 | v2
v5 <- v3 & v2
v5

#Tabela Verdade