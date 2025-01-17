############EXEMPLO 01

Grupo <- c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4)
Resposta <- c(83,91,94,89,89,96,91,92,90,84,91,90,81,83,84,83,88,
              91,89,101,100,91,93,96,95,94,81,78,82,81,77,79,81,80)
kruskal.test(Resposta~Grupo)

####Conclus�o:
# H� evid�ncias para rejeitar H0 (p=0.0001445 < 0,05), ou seja, h�
# evid�ncias de que os grupos apresentam diferentes respostas.




#############EXEMPLO 02

#No conjunto de dados integrado denominado airquality , as medi��es di�rias
#da qualidade do ar em Nova York, de maio a setembro de 1973, s�o registradas. 
#A densidade do oz�nio � apresentada na coluna do quadro de dados Oz�nio .
#Sem assumir que os dados t�m distribui��o normal, teste no n�vel de 
#signific�ncia de 0,05 se a densidade mensal do oz�nio em Nova York tem 
#distribui��es de dados id�nticas de maio a setembro de 1973.

#SOLU��O
head(airquality)
kruskal.test(Ozone ~ Month, data = airquality) 

#A hip�tese nula � que a densidade mensal de oz�nio s�o popula��es id�nticas. 
#Para testar a hip�tese, aplicamos a fun��o kruskal.test para comparar os dados
#mensais independentes. O valor p acaba sendo quase zero (6,901e-06 < 0,05). 
# Portanto, rejeitamos a hip�tese nula.
#No n�vel de signific�ncia de 0,05, conclu�mos que a densidade mensal de 
#oz�nio em Nova York de maio a setembro de 1973 s�o popula��es n�o id�nticas.


###############EXEMPLO 03
baixo<-c(85,90,107,85,100,97,101,64)
medio<-c(78,97,107,80,90,83)
alto<-c(93,100,97,79,97)
QI<-c(baixo,medio,alto)
grupo<-c(rep("b",length(baixo)),rep("m",length(medio)),rep("a",length(alto)))
kruskal.test(QI~grupo)

####Conclus�o:
# N�o h� evid�ncias para rejeitar H0 (p=0,7036 > 0,05), ou seja, n�o h�
# evid�ncias de que os grupos apresentam diferentes escores de QI. 


