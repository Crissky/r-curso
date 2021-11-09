## TESTE DE MANN-WHITNEY

# EXEMPLO 1

# Quantidades de desouros capturados durante a noite
# em 8 armadilhas no habitat A e em 8 armadilhas no habitat B
# Testar a diferença entre as medianas das duas amostras

#Hipóteses
# H0: Não há diferenças entre as medianas das amostras A e B
# H1: Há diferenças entre as medianas das amostras A e B

A <- c(8,12,15,21,25,44,44,60)
B <- c(2,4,5,9,12,17,19)

# Investigação inicial

boxplot(A, B, names = c("Habitat A", "Habitat B"),
        ylab = "Quantidades")

## Teste
wilcox.test(A, B, paired = FALSE,
            alternative = "two.sided")

## Exemplo 2

# Tianeptina: Fámaco antidepressivo
# Rocha (1995) relata os resultados de um ensaio clínico aleatorizado, duplo-cego,
# com o objetivo de comparar a tianeptina com o placebo.

## Hipóteses

# H0: Não há diferenças entre o uso de placebo e o uso de tianeptina
# H1: Há diferenças entre o uso de placebo e o uso de tianeptina.

placebo <- c(6,33,21,26,10,29,33,29,37,15,2,21,7,26,13)
tian <- c(10,8,17,4,17,14,9,4,21,3,7,10,29,13,14,2)

# Investigação inicial
boxplot(placebo, tian, paired = FALSE,
        names = c("Placebo", "Tianeptina"),
        ylab = "Escores")

# Teste
wilcox.test(placebo, tian,
            alternative = "two.sided")

# Rejeitamos H0. Há diferenças entre os dois grupos, ou seja, há diferença
# estatística entre o uso de placebo e o uso de tianeptina.

wilcox.test(placebo, tian, alternative = "less")
