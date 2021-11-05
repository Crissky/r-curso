#Utilize a função getwd para verificar o diretório de trabalho:
getwd()

#setwd muda o diretório de trabalho
#setwd("C:/Users/pg200/Documents/Repositorio/r-curso/aula3")

medicamento <- read.csv("anova1.csv",
                        sep = ";",
                        header = T)
medicamento
View(medicamento)

#aplicar a análise de variância de fator
modeloanova <- aov(HORAS ~ REMEDIO, data = medicamento)
modeloanova

#Mais detalhes do modelo
summary(modeloanova)

#Aplicar a anova de dois fatores
modeloanova2 = aov(HORAS ~ REMEDIO * SEXO,
                   data = medicamento)
modeloanova2

summary(modeloanova2)

#Identificando se existe diferenças entre os grupos
tukey = TukeyHSD(modeloanova2)
tukey
plot(tukey)

#Homogeneidade entre as amostras
install.packages("car") #instalando o pacote
library(car)
leveneTest(HORAS ~ REMEDIO, data = medicamento)

#Normalidade dos resíduos
shapiro.test(resid(modeloanova))
