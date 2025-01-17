######### EXEMPLO 01
fabricante=rep(c('G','F','C'),5)
modelo=rep(c(1:5),each=3)
Consumo<- c(9.0, 11.3,10.6, 
            9.4, 10.9, 10.2,
            8.1, 8.6, 8.8,
            8.3, 8.6, 8.8,
            8.2, 9.2, 9.5)
friedman.test(Consumo, groups=fabricante, blocks=modelo)

#Conclus�o: H� evid�ncias para rejeitar H0 (p= 0.02237 < 0.05), ou seja, h�
#evid�ncias de que o potencial el�trico cut�neo � diferente entre os fabricantes

####################EXEMPLO 02
#A fim de avaliar se houve progress�o na aprendizagem, um professor reteve as
#m�dias de um grupo de 4 alunos no final de cada trimestre. 
#O que pode ser dito?

Trimestre=rep(c('1�','2�','3�'),4)
aluno=rep(c(1:4),each=3)
nota <- c(8,14, 15, 
          15, 17, 16,
          11, 13, 14,
          7, 10, 12)
friedman.test(nota, groups=Trimestre, blocks=aluno)

#Conclus�o: H� evid�ncias para rejeitar H0 (p= 0.03877 < 0.05), ou seja, h�
#evid�ncias de que houve progress�o no processo de aprendizagem dos alunos.
