
library(tidyverse)
library(ggpubr)
library(rstatix)

# Carregando base de dados
library(readxl)
tempo_trabalho <- read_excel("tempo_trabalho.xlsx")
View(tempo_trabalho)

dados <- tempo_trabalho
grupos <- "idade" # Genre
valores <- "tempo" # Days

# 1.3 Estatísticas de resumo
dados %>% 
  group_by(idade) %>% 
  get_summary_stats(tempo, type = "full")


ggboxplot(dados, x = grupos, y = valores)

# Computando o teste

dados %>% 
  group_by(idade) %>% 
  shapiro_test(tempo)

# p = 0.0397 é menor que 0.05, ou seja, é estatisticamente significativo
dados %>% 
  kruskal_test(tempo ~ idade)

# Mas o teste de Dunn apresentou valores de p não significativos.
teste_dunn <- dados %>% 
  dunn_test(tempo ~ idade, p.adjust.method = "bonferroni")


# Calculando o tamanho do efeito
dados %>% 
  kruskal_effsize(tempo ~ idade)
