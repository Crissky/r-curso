tarefas = read.table("tarefa.txt", head=T)
attach(tarefas)
Tarefa

#Teste de Shapiro
shapiro.test(Tarefa) #p-value = 0.1033

#Distribuição normal para comparação
x <- rnorm(50)
x

# Teste de Kolmogorov-Smirnov
ks.test(x, Tarefa) #p-value = 0.1732
