#Teste de Friedman

#1. Pacotes
library(tidyverse)
library(ggpubr)
library(rstatix)

#Carregar os dados
library(readr)
RoundingTimes <- read_csv("RoundingTimes.csv")
View(RoundingTimes)

dados <- RoundingTimes

#Criar uma variável chamada num_jogador
dados <- dados %>%
  mutate(num_jogador = 1:22)

#Converter para o formato longo
names(dados)

dados_longo <- dados %>%
  gather(key = "tipo_rebatida",
         value = "tempo",
         Round.Out, Narrow.Angle, Wide.Angle) %>%
  convert_as_factor(num_jogador, tipo_rebatida)

#3. Estatística Descritiva
estatistica_descritiva <- dados_longo %>%
  group_by(tipo_rebatida) %>%
  get_summary_stats(tempo, type = "full")

#4. Primeira visualização
boxplot_rebatida <- dados_longo %>%
  ggboxplot(x = "tipo_rebatida", y = "tempo",
            bxp.errorbar = TRUE)

boxplot_rebatida

#5. Computando o teste
dados_longo %>%
  group_by(tipo_rebatida) %>%
  shapiro_test(tempo)

dados_longo %>%
  friedman_test(tempo ~ tipo_rebatida | num_jogador)

#Teste foi significativo. Tem diferença no tempo
#dependendo do tipo de rebatida

dados_longo %>%
  friedman_effsize(tempo ~tipo_rebatida | num_jogador)

# Comparações múltiplas
comparacoes_multiplas <- dados_longo %>%
  wilcox_test(tempo ~ tipo_rebatida,
              paired = TRUE,
              p.adjust.method = "bonferroni")
