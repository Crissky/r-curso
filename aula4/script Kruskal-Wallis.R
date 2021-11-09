############EXEMPLO 01

Grupo <- c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4)
Resposta <- c(83,91,94,89,89,96,91,92,90,84,91,90,81,83,84,83,88,
              91,89,101,100,91,93,96,95,94,81,78,82,81,77,79,81,80)
kruskal.test(Resposta~Grupo)

####Conclusão:
# Há evidências para rejeitar H0 (p=0.0001445 < 0,05), ou seja, há
# evidências de que os grupos apresentam diferentes respostas.




#############EXEMPLO 02

#No conjunto de dados integrado denominado airquality , as medições diárias
#da qualidade do ar em Nova York, de maio a setembro de 1973, são registradas. 
#A densidade do ozônio é apresentada na coluna do quadro de dados Ozônio .
#Sem assumir que os dados têm distribuição normal, teste no nível de 
#significância de 0,05 se a densidade mensal do ozônio em Nova York tem 
#distribuições de dados idênticas de maio a setembro de 1973.

#SOLUÇÃO
head(airquality)
kruskal.test(Ozone ~ Month, data = airquality) 

#A hipótese nula é que a densidade mensal de ozônio são populações idênticas. 
#Para testar a hipótese, aplicamos a função kruskal.test para comparar os dados
#mensais independentes. O valor p acaba sendo quase zero (6,901e-06 < 0,05). 
# Portanto, rejeitamos a hipótese nula.
#No nível de significância de 0,05, concluímos que a densidade mensal de 
#ozônio em Nova York de maio a setembro de 1973 são populações não idênticas.


###############EXEMPLO 03
baixo<-c(85,90,107,85,100,97,101,64)
medio<-c(78,97,107,80,90,83)
alto<-c(93,100,97,79,97)
QI<-c(baixo,medio,alto)
grupo<-c(rep("b",length(baixo)),rep("m",length(medio)),rep("a",length(alto)))
kruskal.test(QI~grupo)

####Conclusão:
# Não há evidências para rejeitar H0 (p=0,7036 > 0,05), ou seja, não há
# evidências de que os grupos apresentam diferentes escores de QI. 


