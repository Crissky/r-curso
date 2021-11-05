###############################################
# Anova e Anova com medidas repetidas no R 
# Professora: Elaine Marques
# Monitora: Macileide Oliveira
###############################################

# Utilize a função getwd para verificar o diretório de trabalho:
getwd()
# Utilize a função setwd para mudar o seu diretório de trabalho:
setwd('C:/Users/Elaine/Desktop/CursoAnova')

# Usando seus dados no R
# Se estiver no formato .csv, use:
medicamento <- read.csv("anova1.csv",sep=";", header=T)
medicamento

# Se estiver no formato .txt, use:
# medicamento2 <- read.delim("anova.txt")

# OBS: lembre-se de especificar o argumento sep = ";"
# Caso a configuração do seu computador esteja com separador decimal como ",", 
#chame a tabela da seguinte maneira no R:
#medicamento3 <- read.csv2("anova.csv", dec = ",")

# Visualize sua planilha de dados
View(medicamento)

#Aplicando a Anova para um único fator
#A função do R que executa a ANOVA é a aov ou lm.
# exemplo: aov(V.Dependente ~ V.Independente , data=dados)
modeloanova <- aov(HORAS ~ REMEDIO, data =medicamento)
modeloanova

# Função summary forncece mais informações em relação ao modelo
summary(modeloanova)

###################################################################

#Aplicando a Anova para dois fatores
modeloanova2 =  aov(HORAS ~ REMEDIO * SEXO, data=medicamento)
modeloanova2

summary(modeloanova2)

#Identifique se existe difirença entre os grupos
tukey = TukeyHSD(modeloanova2)
tukey
plot(tukey)

# Testando as premissas da ANOVA

# 1. Homogeneidade das amostras
# Para homogeneidade de variância entre os grupos.
# Para esta premissa é necessário instalar o pacote car
install.packages("car") #instalando o pacote
library(car) #carregando o pacote exigido
leveneTest(HORAS ~ REMEDIO, data =medicamento)

# 2. Normalidade dos resíduos
# A premissa da ANOVA referente a normalidade dos resíduos pode ser 
# testada através do teste de Shapiro-Wilk:
#H0 = distribuição dos dados = normal --> p >0.05
#H1 = distribuição dos dados diferente normal --> p >0.05
shapiro.test(resid(modeloanova))

###################################################

#ANOVA DE MEDIDAS REPETIDAS
avalia<-c(2,5,1,7,2,6,4,7,3,9,4,8,3,5,1,9,6,8,3,6,2,8,1,4)
sujeito<-c(1:6, 1:6, 1:6, 1:6)
juiz<-c(rep("1",6), rep("2",6) , rep("3",6), rep("4",6))
b<-lm(avalia ~as.factor(sujeito) + as.factor(juiz))
anova(b)

#Alpha de Cronbach
install.packages("ltm")
library(ltm)
juiz1<-c(2,5,1,7,2,6)
juiz2<-c(4,7,3,9,4,8)
juiz3<-c(3,5,1,9,6,8)
juiz4<-c(3,6,2,8,1,4)
dados<-data.frame(juiz1,juiz2,juiz3, juiz4)
cronbach.alpha(dados, standardized = FALSE, CI = TRUE, 
               probs = c(0.025, 0.975), B = 1000, na.rm = FALSE)








