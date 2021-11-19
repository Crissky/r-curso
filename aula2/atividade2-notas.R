# 5 Teste t pareado
library(tidyverse)
library(ggpubr)
library(rstatix)

library(readxl)
notas <- read_excel("notas.xlsx")
View(notas)

# nova base de dados notas_long
notas_long <- notas %>%
  gather(key = "momento", value = "nota", antes, depois)
notas_long

# Crie a coluna diferença na base de dados notas,
# para computar a diferença nas notas antes e depois do curso.
notas <- notas %>% 
  mutate(diferenca = depois - antes)

# Teste as suposições para a realização 
# de um teste t pareado.

# As observações não pareadas

notas_long %>%
  group_by(momento) %>%
  get_summary_stats(nota, type = "mean_sd")

notas_long %>%
  ggboxplot(x = "momento",
            y = "nota",
            bxp.errorbar = TRUE,
            add = "jitter")


# Normal se p > 0.05
# O valor de p = 0.114 (normalidade ok)
notas %>%
  shapiro_test(diferenca)

# Não possui Outliers extremos
notas %>%
  identify_outliers(diferenca)

# Execute o teste t pareado.
# Houve diferença nas notas dos alunos
# antes e depois da aplicação da nova metodologia?


# p = 0.129, Não significativo
teste_pareado <- notas_long %>%
  t_test(nota ~ momento, paired = TRUE) %>%
  add_significance()
teste_pareado

# Efeito
notas_long %>%
  cohens_d(nota ~ momento, var.equal = TRUE)


# Gere o gráfico final do teste t pareado 
# usando o ggpaired e também o ggboxplot.
teste_pareado <- teste_pareado %>%
  add_xy_position(x = "momento")

# ggboxplot.
grafico1 <- notas_long %>%
  ggboxplot(x = "momento",
            y = "nota",
            bxp.errorbar = TRUE,
            add = "jitter")

grafico1 +
  stat_pvalue_manual(teste_pareado,
                     tip.length = 0) +
  labs(
    subtitle = get_test_label(teste_pareado,
                              detailed = TRUE),
    x = "Momento", y = "Notas"
  )

# ggpaired
grafico2 <- notas_long %>%
  ggpaired(x = "momento",
           y = "nota",
           xlab = "Momento",
           ylab = "Notas")

grafico2 +
  stat_pvalue_manual(teste_pareado,
                     tip.length = 0) +
  labs(
    subtitle = get_test_label(teste_pareado,
                              detailed = TRUE))
