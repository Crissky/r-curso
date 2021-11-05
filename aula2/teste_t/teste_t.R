# Pacotes
library(tidyverse)
library(ggpubr)
library(rstatix)

# 2. Carregando a base de dados
library(readxl)
genderweight <- read_excel("genderweight.xlsx")


glimpse(genderweight)


genderweight %>%
  sample_n(5)


table(genderweight$group)


genderweight %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")


bxp_peso <- genderweight %>%
  ggboxplot(x = "group", y = "weight")
bxp_peso


bxp_peso <- genderweight %>%
  ggboxplot(x = "group", y = "weight", bxp.errorbar = TRUE)
bxp_peso


bxp_peso <- genderweight %>%
  ggboxplot(x = "group", y = "weight",
            add = "jitter")
bxp_peso


genderweight %>%
  group_by(group) %>%
  identify_outliers(weight)


genderweight %>%
  group_by(group) %>%
  shapiro_test(weight)


genderweight %>%
  ggqqplot(x = "weight", facet.by = "group")


genderweight %>%
  levene_test(weight ~ group)


teste_peso_genero <- genderweight %>%
  t_test(weight ~ group, var.equal = FALSE) %>%
  add_significance()
teste_peso_gênero


genderweight %>%
  cohens_d(weight ~ group, var.equal = FALSE)


genderweight %>%
  cohens_d(weight ~ group, var.equal = FALSE)
