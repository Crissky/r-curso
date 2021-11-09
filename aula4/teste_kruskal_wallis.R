#Teste de Kruskal-Wallis

#1. Pacotes
library(tidyverse)
library(ggpubr)
library(rstatix)

#2. Base de Dados
library(readxl)
FilmData <- read_excel("FilmData.xlsx")
View(FilmData)
glimpse(FilmData)

#Criando uma cópia dos dados originais
dados  <- FilmData

#3. Estatística Descritiva
estatistica_descritiva <- dados  %>%
  group_by(Genre) %>%
  get_summary_stats(Days, type = "full")

View(estatistica_descritiva)

#4. Primeira Visualização
ggboxplot(dados , x = "Genre", y = "Days",
          bxp.errorbar = TRUE, add = "jitter")

#5. Computando o teste
# teste de normalidade
dados %>%
  group_by(Genre) %>%
  shapiro_test(Days)

#Os dados não seguem uma distribuição normal.
#Vamos usar um teste não paramétrico

resultado_kruskal_wallis <- dados %>%
  kruskal_test(Days ~ Genre)

resultado_kruskal_wallis

#H0: Não há diferença
#H1: Há diferença em pelo menos dois grupos.

# p = 0.262 -> Não houve diferença estatisticamente
#significativa no número de dias que os filmes passam
#no cinema, em relação ao gênero.

#Se p < 0.05, faríamos uma comparação dois a dois

#Teste de Dunn com correção de bonderroni
resultado_post_hoc <- dados %>%
  dunn_test(Days ~ Genre,
            p.adjust.method = "bonferroni")

#Tamanho do efeito
dados %>%
  kruskal_effsize(Days ~ Genre)
