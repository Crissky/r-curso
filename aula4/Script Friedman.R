######### EXEMPLO 01
fabricante=rep(c('G','F','C'),5)
modelo=rep(c(1:5),each=3)
Consumo<- c(9.0, 11.3,10.6, 
            9.4, 10.9, 10.2,
            8.1, 8.6, 8.8,
            8.3, 8.6, 8.8,
            8.2, 9.2, 9.5)
friedman.test(Consumo, groups=fabricante, blocks=modelo)

#Conclusão: Há evidências para rejeitar H0 (p= 0.02237 < 0.05), ou seja, há
#evidências de que o potencial elétrico cutâneo é diferente entre os fabricantes

####################EXEMPLO 02
#A fim de avaliar se houve progressão na aprendizagem, um professor reteve as
#médias de um grupo de 4 alunos no final de cada trimestre. 
#O que pode ser dito?

Trimestre=rep(c('1º','2º','3º'),4)
aluno=rep(c(1:4),each=3)
nota <- c(8,14, 15, 
          15, 17, 16,
          11, 13, 14,
          7, 10, 12)
friedman.test(nota, groups=Trimestre, blocks=aluno)

#Conclusão: Há evidências para rejeitar H0 (p= 0.03877 < 0.05), ou seja, há
#evidências de que houve progressão no processo de aprendizagem dos alunos.
