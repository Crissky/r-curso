# Teste t pareado

#0. Preparar o ambiente

library(tidyverse)
library(ggpubr)
library(rstatix)

#install.packages(c("tidyverse", "ggpubr", "rstatix"))

#1. Carregando base de dados

#Nessa parte iremos carregar nossa base de dados

library(readxl)
mice2 <- read_excel("mice2.xlsx")
View(mice2)

# Transforma os dados para o formato longo
mice2.long <- mice2 %>%
  gather(key = "group", value = "weight",
         before, after)
head(mice2.long)


#2. Estatística de resumo
mice2.long %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "mean_sd")

#3. Visualização
bxp_pareado <- ggpaired(mice2.long,
                        x = "group",
                        y = "weight",
                        order = c("before", "after"),
                        ylab = "Weight(g)",
                        xlab = "Group"
)
bxp_pareado

#4. Suposicoes do teste
# Iremos acrescentar uma coluna chamada diferenças

mice2 <- mice2 %>%
  mutate(diferencas = after - before)
mice2

#4.1 normalidade:
# p>0.05 (normalidade)

mice2 %>%
  shapiro_test(diferencas)

#4.2 Outliers Extremos
mice2 %>%
  identify_outliers(diferencas)

#5 Computando o teste
teste_pareado <- mice2.long %>%
  t_test(weight ~ group, paired = TRUE) %>%
  add_significance()

teste_pareado
View(teste_pareado)

#Tamanho do efeito
mice2.long %>%
  cohens_d(weight ~ group, paired = TRUE)

#6. Reportando o resultado
teste_pareado <- teste_pareado %>%
  add_xy_position(x="group")

bxp_pareado +
  stat_pvalue_manual(teste_pareado,
                     tip.length = 0) +
  labs(subtitle = get_test_label(teste_pareado,
                                 detailed = TRUE))
