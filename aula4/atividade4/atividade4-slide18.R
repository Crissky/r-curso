library(tidyverse)
library(ggpubr)
library(rstatix)

# Carregando base de dados
library(readxl)
arco_e_flecha <- read_excel("arco_e_flecha.xlsx")
View(arco_e_flecha)

dados <- arco_e_flecha

# 1.3 Estatísticas de resumo
dados %>% 
  group_by(jogador) %>% 
  get_summary_stats(pontuacao, type = "full")


ggboxplot(dados, x = "jogador", y = "pontuacao")

# Computando o teste

dados %>% 
  group_by(jogador) %>% 
  shapiro_test(pontuacao)

# p = 0.698 é maior que 0.05, ou seja, não é estatisticamente significativo
dados %>% 
  kruskal_test(pontuacao ~ jogador)

# O teste de Dunn também apresentou como resultados não significativo
teste_dunn <- dados %>% 
  dunn_test(pontuacao ~ jogador, p.adjust.method = "bonferroni")


# Calculando o tamanho do efeito
dados %>% 
  kruskal_effsize(pontuacao ~ jogador)
