notas <- read.table("notas.txt", head=T)
attach(notas) #ctrl+R para executar#
Notas
shapiro.test(Notas)