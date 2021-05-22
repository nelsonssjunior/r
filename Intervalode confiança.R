#Remove objetos da memória do R
rm(list=ls(all=TRUE)) 

###########################################################
####### Intervalo de Confiança #######
## AED - Capitulo 03 - Prof. Máiron Chaves ####
###########################################################

########## Intervalo de confiança para média amostral pela distribuição Normal Padrão #############################

'Obter o intervalo de confiança para uma variável cuja média = 30, desvio padrão = 7,31 e n = 30
Temos que definir o nível de confiança do nosso intervalo.
Podemos obter o valor do quantil para o nível de confiança desejado com a função qnorm()
O quantil na distribuição normal padrão para 95% de confiança'
ic <- 0.95
alfa <- 1-ic
1-(alfa/2)
qnorm(0.975)

#Vamos armazenar os valores em objetos
media <- 30
desvio_padrao_populacional <- 7.31
n <- 30
quantil_95 <- qnorm(0.975)

#Aplicando a fórmula vista na apostila fica:
Limite_Superior <- 30+quantil_95*(desvio_padrao_populacional/sqrt(n))
Limite_Inferior <- 30-quantil_95*(desvio_padrao_populacional/sqrt(n))
paste("Com 95% de confiança, podemos afirmar que a média varia entre",Limite_Inferior," e ",Limite_Superior)

####################### Intervalo de confiança para a média amostral pela distribuição t de Student ############


'A teoria nos diz para utilizar a distribuição t de Student quando não soubermos o desvio padrão populacional.
Vamos assumir que o desvio padrão que temos é obtido a partir da amostra
Vamos armazenar os valores em objetos'
media <- 30
desvio_padrao_amostral <- 7.31
n <- 30
quantil_95_t <- qt(0.975,df = n-1)


#Aplicando a fórmula vista na apostila fica:
Limite_Superior_t <- 30+quantil_95_t*(desvio_padrao_amostral/sqrt(n))
Limite_Inferior_t <- 30-quantil_95_t*(desvio_padrao_amostral/sqrt(n))
paste("Com 95% de confiança, podemos afirmar que a média varia
entre",Limite_Inferior_t," e ",Limite_Superior_t)


'Supondo que nossa variável já esteja em um data frame aqui no R, tem um comando para fornecer 
o intervalo de confiança de forma bem mais fácil.
Vamos gerar com o comando rnorm() uma variável aleatoria com média 30, desvio padrão 7,31 e n = 30'
va <- rnorm(n = 30, mean = 30, sd = 7.31)

#Vamos visualizar a va gerada
hist(va)

#Calculando o intervalo de 95% de confiança com a distribuição t de Student
#com a funçao t.test()
IC <-t.test(va, conf.level = 0.95)
IC$conf.int

#Pronto, já temos o intervalo de confiança para média. Beeem mais fácil assim :)


####################### Intervalo de confiança para a proporção ############


'Utilizando o exemplo da apostila, onde calculamos o intervalo para proporção onde
138 de n = 500 clientes realizaram a devolução do produto
Vamos armazenar os valores em objetos'


devolucoes <- 138
n <- 500
quantil_95 <-qnorm(0.975)
proporcao_devolucoes <- devolucoes/n

#Aplicando a fórmula vista na apostila fica:
Limite_Superior_prop <-
  proporcao_devolucoes + quantil_95 *
  sqrt(proporcao_devolucoes*(1-proporcao_devolucoes)/n)

Limite_Inferior_prop <-
  proporcao_devolucoes - quantil_95 *
  sqrt(proporcao_devolucoes*(1-proporcao_devolucoes)/n)
paste("Com 95% de confiança, podemos afirmar que a proporção varia
entre",Limite_Inferior_prop," e ",Limite_Superior_prop)


#Podemos obter o intervalo de confiança para proporção mais fácil pela função
prop.test()
IC_proporcao <- prop.test(x = 138, n = 500, conf.level = 0.95)
IC_proporcao$conf.int


############### Intervalo de confiança para média via Bootstrap ############
#Vamos gerar uma va seguindo uma distribuição qui-quadrado
va <- rchisq(n = 60, df = 3)

#Observe o quão assimétrica é a va
hist(va)

#Inicializa variaveis
medias <- c() #Essa variável é um vetor para armazenar a média de cada subamostra bootstrap
R <- 1000 #Numero de subamostras extraídas para gerar a distribuição amostral de médias

#bootstrap
for (i in 1:R) {
  #Realiza uma subamostragem aleatória com reposição da va
  reamostra <- sample(va, size = 50, replace = T)
  #Armazena a média da subamostra
  medias[i] <- mean(reamostra)
}

#Distribuicao das médias das subamostras (distribuição amostral da média da va)
hist(medias)

'Observe que mesmo a variável original não seguindo uma distribuição normal, o Teorema Central do Limite
nos garante que a distribuição das médias será normal se n é suficientemente grande
A partir das médias geradas, precisamos achar dois valores, o que corta a cauda inferior
e o que corta a cauda superior da distribuição. Lembrando que ela é simétrica
Caso o intervalo desejado seja de 95% de confiança, temos que ordenar essa distribuição
do menor valor para o maior e achar o valor que deixará 2,5% dos dados para trás e o valor que deixará 97,5% para trás'
(1-0.95)/2  
1-(1-0.95)/2

#Visualize o intervalo de confiança via bootstrap
quantile( medias, probs = c(0.025,0.975))

#Vamos realizar mais um experimento
#Geraremos uma va com média = 30 e desvio padrão amostral =7.31 e n = 30
va <- rnorm(n = 30, mean = 30, sd = 7.31)

'Iremos calcular o intervalo de confiança usando o Bootstrap e também com a distribuição
 t de Student. Compararemos os resultados.
Inicializa variavel para armazenar as médias de cada subamostra'
medias <- c()

R <- 10000 #Numero de subamostras extraídas para gerar a distribuição amostral de médias

#bootstrap
for (i in 1:R) {
  #Realiza uma subamostragem aleatória com reposição da va
  reamostra <- sample(va, size = 20, replace = T)
  #Armazena a média da subamostra
  medias[i] <- mean(reamostra)
}

#Distribuicao das médias das subamostras (distribuição amostral da média da va)
hist(medias)

#Limites inferior e superior do intervalo pelo bootstrap
quantile( medias, probs = c(0.025,0.975))

#Limites inferior e superior do intervalo via t de Student
IC<-t.test(va, conf.level = 0.95)
IC$conf.int
