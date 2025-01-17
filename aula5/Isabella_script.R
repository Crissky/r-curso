####### TESTE DE WILCOXON ######

# Exemplo 1 ----------

# A massa de 10 p�ssaros migrat�rios foi medida em duas ocasi�es, 
# primeiro em agosto e os mesmos p�ssaros (marcados individualmente e recapturados) 
# foram medidos novamente em setembro
# O peso dos p�ssaros mudou?

# Hip�teses------
#H0: N�o houve diferen�a entre os pesos.
#H1: Houve diferen�a entre os pesos.

ago <- c(10.3,11.4,10.9,12.0,10.0,11.9,12.2,12.3,11.7,12.0)
set <- c(12.2,12.1,13.1,11.9,12.0,12.9,11.4,12.1,13.5,12.3)

massa <- data.frame(ago,set)
names(massa) <- c("Agosto","Setembro")

## Investiga��o inicial --------

boxplot(massa)

## Teste -------

wilcox.test(ago, set, paired = TRUE, alternative = "two.sided")

# H� uma diferen�a significativa entre as massas medianas das duas amostras
# e os p�ssaros t�m uma massa significativamente maior em setembro.


## Testar se os pesos aumentaram ------

wilcox.test(ago, set, paired = TRUE, alternative = "greater")

# N�o rejeita H0.


# Exemplo 2 -------

# Pares combinados de tempos (em segundos) de uma amostra aleat�ria de crian�as �s quais
# foram dados blocos com a instru��o de constru�rem a torre mais alta poss�vel.
# Esse procedimento � usado pra medir a intelig�ncia de crian�as.
# Testar a afirmativa de que n�o h� diferen�a entre os tempos da primeira e da segunda tentativas.

## Hip�teses: -------
#H0: N�o h� diferen�a entre os tempos da primeira e da segunda tentativas.
#H1: H� uma diferen�a entre os tempos da primeira e segunda tentativas.


tent1 <- c(30,19,19,23,29,178,42,20,12,39,14,81,17,31,52)
tent2 <- c(30,6,14,8,14,52,14,22,17,8,11,30,14,17,15)

tempos <- data.frame(tent1, tent2)
names(tempos) <- c("Tentativa 1", "Tentativa 2")

## Investiga��o inicial ------

boxplot(tempos)
boxplot(tempos, ylim = c(0,70))


## Teste -------

wilcox.test(tent1, tent2, paired = T, alternative = "two.sided")

# Rejeita-se a hip�tese nula. Parece haver uma diferen�a entre os tempos da primeira 
# e da segunda tentativas.

## Verificar se os tempos diminu�ram ------

wilcox.test(tent1, tent2, paired = T, alternative = "less")
# N�o rejeita H0.


######## TESTE DE MANN-WHITNEY ########

# Exemplo 1 -------

# Quantidades de besouros capturados durante a noite
# em 8 armadilhas no habitat A e em 7 armadilhas no habitat B
# Testar a diferen�a entre as medianas das duas amostras.

## Hip�teses ------
# H0: N�o h� diferen�a entre as medianas das amostras A e B.
# H1: H� diferen�a entre as medianas das amostras A e B.

A <- c(8,12,15,21,25,44,44,60)
B <- c(2,4,5,9,12,17,19)

## Investiga��o inicial -----

boxplot(A,B, names = c("Habitat A", "Habitat B"), ylab = "Quantidades")

## Teste --------

wilcox.test(A, B, alternative = "two.sided")


# Exemplo 2 -------

# Tianeptina: f�rmaco antidepressivo
# Rocha (1995) relata os resultados de um ensaio cl�nico aleatorizado, duplo-cego, 
# com o objetivo de comparar a tianeptina com o placebo. 

## Hip�teses ------

# H0: N�o h� diferen�a entre o uso de placebo e o uso de tianeptina.
# H1: H� diferen�a entre o uso de placebo e o uso de tianeptina.


placebo <- c(6,33,21,26,10,29,33,29,37,15,2,21,7,26,13)
tian <- c(10,8,17,4,17,14,9,4,21,3,7,10,29,13,14,2)

## Investiga��o inicial -------

boxplot(placebo, tian, names = c("Placebo", "Tianeptina"), ylab = "Escores")


## Teste ------

wilcox.test(placebo, tian, alternative = "two.sided")

# Rejeitamos H0. H� diferen�a entre os dois grupos, ou seja, h� diferen�a
# estat�stica entre o uso de placebo e o uso de tianeptina. 


## Testar se os escores de depress�o diminu�ram com a tianeptina-----
wilcox.test(placebo, tian, alternative = "less")

#n�o rejeita H0.

