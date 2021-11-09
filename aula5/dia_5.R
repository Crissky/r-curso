# A massa de 10 pássaros migratórios foi medida em duas ocasiões
# primeiro em agosto e os mesmos pássaros (marcados individualemente e recapturados)
# foram medidos novamente em setembro
# O peso dos pássaros mudou?

# Hipóteses
# H0: Não houve diferença entre os pesos
# H1: Houve diferença entre os pesos

ago <- c(10.3,11.4,10.9,12.0,10.0,11.9,12.2,12.3,11.7,12.0)
set <- c(12.2,12.1,13.1,11.9,12.0,12.9,11.4,12.1,13.5,12.3)

massa <- data.frame(ago, set)
names(massa) <- c("Agosto", "Setembro")

## Investigação inicial

boxplot(massa)

## Teste

wilcox.test(ago, set, paired = TRUE,
            alternative = "two.sided")

# Há uma diferença significativa entre as massas medianas das duas amostras
# e os pássaros têm uma massa significativamente maior em setembro.

# Testar se os pesos aumentaram

wilcox.test(ago, set, paired = TRUE,
            alternative = "greater")

# Rejeita H0

# Exemplo 2

# Pares combinados de tempos (segundos) de uma amostra aleatória de crianças às quais
# foram dados blocos com a instrução de construirem a torre mais alta possível.
# Esse procedimente é usado para medir a inteligência de crianças.
# Testar a afirmativa de que não há diferença entre os tempos da primeira e da segunda tentativas.

# Hipóteses
# H0: Não há diferença entre os tempos da primeira e da sengunda tentativas.
# H1: Há uma diferença entre os tempos da primeira e da sengunda tentativas.

tent1 <- c(30,19,19,23,29,178,42,20,12,39,14,81,17,31,52)
tent2 <- c(30,6,14,8,14,52,14,22,17,8,11,30,14,17,15)

tempos <- data.frame(tent1, tent2)
names(tempos) <- c("Tentativa 1", "Tentativa 2")

# Investigação inicial

boxplot(tempos)
boxplot(tempos, ylim = c(0,70))

## Teste
wilcox.test(tent1, tent2, paired = TRUE,
            alternative = "two.sided")

# Rejeita-se a hipótese nula. Parece haver uma diferença entre os tempos da primeira
# e da segunda tentativas.

## Verificar se diminuiu o tempo
wilcox.test(tent1, tent2, paired = TRUE,
            alternative = "less")
