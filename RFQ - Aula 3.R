#Curso de R para Finanças Quantitativas
#Aula 3 - Missing Values e Strings
#https://www.outspokenmarket.com/rfinancasquantitativas.html
#Leandro Guerra

#Missings - NA
 x <- c(1:3,NA)
x
is.na(x)
sum(is.na(x))
x <- na.omit(x)


#Vetores de caracteres - Strings
x<- c("Um", "Dois")
x
length(x)
x[1]
x[2]

pos1 <- x[2]
pos1

string <- paste(c(x,"Tres"),sep="")
string
