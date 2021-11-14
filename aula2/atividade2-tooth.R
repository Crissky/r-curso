# 1 Informações sobre a base de dados

# 2 Preparando o ambiente
library(tidyverse)
library(ggpubr)
library(rstatix)

library(readxl)

# 3 Carregando a base de dados.
ToothGrowth <- read_excel("ToothGrowth.xlsx")
View(ToothGrowth)

glimpse(ToothGrowth)

# Criando Tabela
table(ToothGrowth$supp)

# Contagem supp
# OJ = 30
# VC = 30
Supp <- ToothGrowth["supp"]
Supp %>%
  group_by(supp) %>%
  summarise(count=n())

# 4 Teste t
# 4.1 Estatística descritiva

ToothGrowth %>% 
  group_by(supp) %>% 
  get_summary_stats(len,type = "mean_sd")

# 4.2 Primeira Visualização

ToothGrowth %>%
  ggboxplot(x = "supp",
            y = "len",
            bxp.errorbar = TRUE,
            add = "jitter")

# 4.3 Suposições do teste

# Não ficou muito claro se cada indivíduo
# aparece somente uma vez nessas observações,
# pois como o estudo é sobre o crescimento dos dente,
# espera-se que haja, ao menos, uma medição inicial e 
# uma medição final.
# Mas, caso essa seja somente a medição inicial, podemos
# dizer que são observações independetes, 
# já que a medição de um indivíduo não interfere a do outro.


# Nenhum valor ultrapassou as barras de erro.
ToothGrowth %>%
  group_by(supp) %>%
  identify_outliers(len)

# Shapiro-Wilk

# O valor de p encontrado no teste de shapiro
# foi de 0.0236 para o supp OJ
# e 0.428 para o supp VC.
# Sendo rejeitada a hipótese de normalidade para 
# valores menores que 0.05.
ToothGrowth %>%
  group_by(supp) %>%
  shapiro_test(len)

# QQPLOT
# Os valores estão dentro da área sombreada,
# que indica normalidade
ToothGrowth %>%
  ggqqplot(x = "len", facet.by = "supp")

# Cheque se ambos os grupos de suplemento
# tem variâncias iguais.
# p = 0.275, ou seja, maior que 0.05, indicando que
# as variancias são iguais.
ToothGrowth %>%
  levene_test(len ~ supp)

# Teste t
# p = 0.0604 é maior que 0.05, 
# o que indica que não há diferença estatística.
resultado_teste_t <- ToothGrowth %>%
  t_test(len ~ supp, var.equal = TRUE) %>%
  add_significance()
resultado_teste_t

# Finalizando a visualização inicial modificando
# o título dos eixos e adicionando as
# informações do teste realizado.

resultado_teste_t <- resultado_teste_t %>%
  add_xy_position(x = "supp")

grafico <- ToothGrowth %>%
  ggboxplot(x = "supp",
            y = "len",
            bxp.errorbar = TRUE,
            add = "jitter")

grafico +
  stat_pvalue_manual(resultado_teste_t,
                     tip.length = 0) +
  labs(
    subtitle = get_test_label(resultado_teste_t,
                              detailed = TRUE),
    x = "Suplemento", y = "Tamanho"
  )


# A média do comprimento para o suplemento "OJ"
# foi de 20,7 com dp de 6,61, já o suplemento 
# "VC" teve uma média de comprimeto 17,0 
# com dp de 8,27.
# O teste t para os dois grupos 
# não foi estatíticamente significativo,
# t(58) = 1,92, p > 0,05; em que 
# t(58) denota a estatística de 
# teste que teve 58 graus de liberdade.

