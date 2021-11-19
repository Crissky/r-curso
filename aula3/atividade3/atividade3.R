library(tidyverse)
library(ggpubr)
library(rstatix)


automoveis <- data.frame(modelo1 = c(254, 263, 241 ,237, 251),
                           modelo2 = c(234, 218 , 235, 227, 216),
                           modelo3 = c(200, 222, 197, 206, 204),
                           stringsAsFactors = FALSE)
automoveis

dados <- automoveis
dados_longo <- dados %>%
  gather(key = "modelo",
         value = "valor", modelo1, modelo2, modelo3)

# ANOVA
# p = 4.99e-05 < que 0.05, ou seja,
# há diferença entre as varianças.

modelo_anova <- aov(valor ~ modelo, dados_longo)
modelo_anova
summary(modelo_anova)

# Tukey
# Com o teste de Tukey é possível observar
# que há diferenças entre as variancia dos três modelos
TukeyHSD(modelo_anova)

