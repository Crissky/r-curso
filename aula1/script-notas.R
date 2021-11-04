notas <- read.table("notas.txt", head=T)
attach(notas) # ctrl+R para executar
Notas
shapiro.test(Notas)
x <- rnorm(50)
x
ks.test(x, Notas)
ks.test(Notas, x)


#x <- rnorm(length(Notas))
#x
#ks.test(x, Notas)
#ks.test(Notas, x)
