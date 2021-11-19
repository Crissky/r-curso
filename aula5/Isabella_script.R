####### TESTE DE WILCOXON ######

# Exemplo 1 ----------

# A massa de 10 pássaros migratórios foi medida em duas ocasiões, 
# primeiro em agosto e os mesmos pássaros (marcados individualmente e recapturados) 
# foram medidos novamente em setembro
# O peso dos pássaros mudou?

# Hipóteses------
#H0: Não houve diferença entre os pesos.
#H1: Houve diferença entre os pesos.

ago <- c(10.3,11.4,10.9,12.0,10.0,11.9,12.2,12.3,11.7,12.0)
set <- c(12.2,12.1,13.1,11.9,12.0,12.9,11.4,12.1,13.5,12.3)

massa <- data.frame(ago,set)
names(massa) <- c("Agosto","Setembro")

## Investigação inicial --------

boxplot(massa)

## Teste -------

wilcox.test(ago, set, paired = TRUE, alternative = "two.sided")

# Há uma diferença significativa entre as massas medianas das duas amostras
# e os pássaros têm uma massa significativamente maior em setembro.


## Testar se os pesos aumentaram ------

wilcox.test(ago, set, paired = TRUE, alternative = "greater")

# Não rejeita H0.


# Exemplo 2 -------

# Pares combinados de tempos (em segundos) de uma amostra aleatória de crianças às quais
# foram dados blocos com a instrução de construírem a torre mais alta possível.
# Esse procedimento é usado pra medir a inteligência de crianças.
# Testar a afirmativa de que não há diferença entre os tempos da primeira e da segunda tentativas.

## Hipóteses: -------
#H0: Não há diferença entre os tempos da primeira e da segunda tentativas.
#H1: Há uma diferença entre os tempos da primeira e segunda tentativas.


tent1 <- c(30,19,19,23,29,178,42,20,12,39,14,81,17,31,52)
tent2 <- c(30,6,14,8,14,52,14,22,17,8,11,30,14,17,15)

tempos <- data.frame(tent1, tent2)
names(tempos) <- c("Tentativa 1", "Tentativa 2")

## Investigação inicial ------

boxplot(tempos)
boxplot(tempos, ylim = c(0,70))


## Teste -------

wilcox.test(tent1, tent2, paired = T, alternative = "two.sided")

# Rejeita-se a hipótese nula. Parece haver uma diferença entre os tempos da primeira 
# e da segunda tentativas.

## Verificar se os tempos diminuíram ------

wilcox.test(tent1, tent2, paired = T, alternative = "less")
# Não rejeita H0.


######## TESTE DE MANN-WHITNEY ########

# Exemplo 1 -------

# Quantidades de besouros capturados durante a noite
# em 8 armadilhas no habitat A e em 7 armadilhas no habitat B
# Testar a diferença entre as medianas das duas amostras.

## Hipóteses ------
# H0: Não há diferença entre as medianas das amostras A e B.
# H1: Há diferença entre as medianas das amostras A e B.

A <- c(8,12,15,21,25,44,44,60)
B <- c(2,4,5,9,12,17,19)

## Investigação inicial -----

boxplot(A,B, names = c("Habitat A", "Habitat B"), ylab = "Quantidades")

## Teste --------

wilcox.test(A, B, alternative = "two.sided")


# Exemplo 2 -------

# Tianeptina: fármaco antidepressivo
# Rocha (1995) relata os resultados de um ensaio clínico aleatorizado, duplo-cego, 
# com o objetivo de comparar a tianeptina com o placebo. 

## Hipóteses ------

# H0: Não há diferença entre o uso de placebo e o uso de tianeptina.
# H1: Há diferença entre o uso de placebo e o uso de tianeptina.


placebo <- c(6,33,21,26,10,29,33,29,37,15,2,21,7,26,13)
tian <- c(10,8,17,4,17,14,9,4,21,3,7,10,29,13,14,2)

## Investigação inicial -------

boxplot(placebo, tian, names = c("Placebo", "Tianeptina"), ylab = "Escores")


## Teste ------

wilcox.test(placebo, tian, alternative = "two.sided")

# Rejeitamos H0. Há diferença entre os dois grupos, ou seja, há diferença
# estatística entre o uso de placebo e o uso de tianeptina. 


## Testar se os escores de depressão diminuíram com a tianeptina-----
wilcox.test(placebo, tian, alternative = "less")

#não rejeita H0.

