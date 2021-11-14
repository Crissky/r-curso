library(tidyverse)
library(ggpubr)
library(rstatix)

library(readxl)
velocidade_atletas <- read_excel("velocidade_atletas.xlsx")

dados <- velocidade_atletas

# Convertendo longo
dados_longo <- dados %>%
  gather(key = "trecho",
         value = "velocidade",
         trecho_a, trecho_b, trecho_c, trecho_d) %>%
  convert_as_factor(atleta, trecho)

# Estatística Descritiva
dados_longo %>%
  group_by(trecho) %>%
  get_summary_stats(velocidade, type = "full")

# Visualização
dados_longo %>%
  ggboxplot(x = "trecho", y = "velocidade")




# Computando o teste
dados_longo %>%
  group_by(trecho) %>%
  shapiro_test(velocidade)

# p = 0.005533 é menor que 0.05, ou seja, foi significativo.
teste_friedman <- dados_longo %>%
  friedman_test(velocidade ~ trecho | atleta)

# Tamanho do efeito
dados_longo %>%
  friedman_effsize(velocidade ~ trecho | atleta)

# Comparações Múltiplas
# Mas as comparações dois a dois não apresentaram p valor significativo.
comparacoes <- dados_longo %>%
  wilcox_test(velocidade ~ trecho,
              paired = TRUE,
              p.adjust.method = "bonferroni")


# Gráfico final
comparacoes <- comparacoes %>% 
  add_xy_position(x = "trecho")

comparacoes$y.position <- c(7, 6.6, 6.5)

ggboxplot(dados_longo, x = "trecho", 
          y = "velocidade", 
          add = "jitter",
          bxp.errorbar = TRUE) +
  stat_pvalue_manual(comparacoes, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(teste_friedman, type = "expression", detailed = TRUE),
    caption = get_pwc_label(comparacoes, type = "expression"),
    x = "Trechos", y = "Velocidades"
  )
