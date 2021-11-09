# Pacotes
library(rio)
library(tidyverse)
library(gridExtra)
library(ggplot2)


# Teste Wilcoxon
## Exemplo 1: amostra pequena

seed <- import(file = "http://lea.estatistica.ccet.ufrn.br/tutoriais/dados/seed.xls")
dados_seed <- dados <- data.frame(Semente = c(rep("Normal",11), rep("Seco",11)),
                                  Rendimento = c(seed$'Regular seed',seed$'Kiln-dried seed'))

Seca <- dados_seed[dados_seed$Semente == "Seco",]
Normal <- dados_seed[dados_seed$Semente == "Normal",]
completo <- data.frame(Normal,Seca)
completo

ggplot(dados_seed, aes(x = Semente, y = Rendimento)) + geom_boxplot() +
  labs(x = "Semente", y = "Rendimento", title =
         "Boxplot para o rendimento dos tipos de sementes") + theme_bw()

ggplot(dados_seed, aes(Rendimento, fill= Semente)) + geom_density(alpha=.5) +
  labs(title="Densidade do Rendimento por semente", x="Rendimento", y="Densidade")

wilcox.test(Normal$Rendimento,Seca$Rendimento,paired = T,alternative =
              "two.sided", conf.level = 0.95)


## Exemplo 2: amostras grandes
Rendimento1 <- rnorm(500, 1300, 80)
Rendimento2 <- rnorm(500, 1700, 100)
Seed <- data.frame(Semente = c(rep("Normal",500), rep("Seco",500)),
                   Rendimento = c(Rendimento1, Rendimento2))

ggplot(Seed, aes(x = Semente, y = Rendimento)) + geom_boxplot() +
  labs(x = "Semente", y = "Rendimento", title =
         "Boxplot para o rendimento dos tipos de sementes") + theme_bw()

ggplot(Seed, aes(Rendimento, fill= Semente)) + geom_density(alpha=.5) +
  labs(title="Densidade do Rendimento por semente", x="Rendimento", y="Densidade")


wilcox.test(Rendimento1, Rendimento2, paired = T, alternative =
              "two.sided", conf.level = 0.95)

# Teste de Mann-Whitney
## Exemplo 1: amostra pequena

A <- c(5,7,8,8,4,8,6,7,3,8)
B <- c(9,6,8,7,6,9,7,7,8,6)
Refrigerantes <- data.frame(Marca = c(rep("A",10), rep("B",10)),
                            Nota = c(A, B))
Refrigerantes


ggplot(Refrigerantes, aes(x = Marca, y = Nota)) + geom_boxplot() +
  labs(x = "Marca", y = "Nota", title =
         "Boxplot para as notas de cada refrigerante") + theme_bw()


ggplot(Refrigerantes, aes(Nota, fill= Marca)) + geom_density(alpha=.5) +
  labs(title="Densidade das notas por Refrigerante", x="Notas", y="Densidade")


wilcox.test(A,B, paired = FALSE, alternative = "two.sided", conf.level = 0.95)

## Exemplo 2: amostra grande

A2 <- rnorm(2000, 8, 0.5)
B2 <- rnorm(2000, 7.5, 1)
Refrigerantes2 <- data.frame(Marca = c(rep("A",2000), rep("B",2000)),
                             Nota = c(A2, B2))
Refrigerantes2[1:10,]


ggplot(Refrigerantes2, aes(x = Marca, y = Nota)) + geom_boxplot() +
  labs(x = "Marca", y = "Nota", title =
         "Boxplot para as notas de cada refrigerante") + theme_bw()

ggplot(Refrigerantes2, aes(Nota, fill= Marca)) + geom_density(alpha=.3) +
  labs(title="Densidade das notas por Refrigerante", x="Notas", y="Densidade")


wilcox.test(A2,B2, paired = FALSE, alternative = "two.sided", conf.level = 0.95)

