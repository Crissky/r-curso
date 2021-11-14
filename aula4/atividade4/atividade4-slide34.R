library(tidyverse)
library(ggpubr)
library(rstatix)

library(readxl)
tensao_emocional <- read_excel("tensao_emocional.xlsx")
View(tensao_emocional)

dados <- tensao_emocional

# Convertendo longo
dados_longo <- dados %>%
  gather(key = "sentimento",
         value = "medicao",
         Medo, Alegria, Tristeza, Calma) %>%
  convert_as_factor(sujeito, sentimento)


# Estatística Descritiva
dados_longo %>%
  group_by(sentimento) %>%
  get_summary_stats(medicao, type = "full")

# Visualização
dados_longo %>%
  ggboxplot(x = "sentimento", y = "medicao")


# Computando o teste
dados_longo %>%
  group_by(sentimento) %>%
  shapiro_test(medicao)

# p = 0.0917 é maior que 0.05, ou seja, não foi significativo.
teste_friedman <- dados_longo %>%
  friedman_test(medicao ~ sentimento | sujeito)
  
# Tamanho do efeito
dados_longo %>%
  friedman_effsize(medicao ~ sentimento | sujeito)

# Comparações Múltiplas
# As comparações dois a dois também não apresentaram p valor significativo.
comparacoes <- dados_longo %>%
  wilcox_test(medicao ~ sentimento,
              paired = TRUE,
              p.adjust.method = "bonferroni")


# Gráfico final
comparacoes <- comparacoes %>% 
  add_xy_position(x = "sentimento")

comparacoes$y.position <- c(7, 6.6, 6.5)

ggboxplot(dados_longo, x = "sentimento", 
          y = "medicao", 
          add = "jitter",
          bxp.errorbar = TRUE) +
  stat_pvalue_manual(comparacoes, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(teste_friedman, type = "expression", detailed = TRUE),
    caption = get_pwc_label(comparacoes, type = "expression"),
    x = "Sentimentos", y = "Medição"
  )
