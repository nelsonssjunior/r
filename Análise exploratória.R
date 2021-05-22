####### Análise exploratória de dados #######

#Remove objetos da memória do R
rm(list=ls(all=TRUE)) 

#Cria o data frame contendo o historico de vendas do cafe
dados <- data.frame(Vendas_Cafe = c(18, 20, 23, 23, 23, 23, 24, 25, 26, 26, 26, 26,
                                    27, 28, 28,
                                    29, 29, 30, 30, 31, 31, 33, 34, 35, 38, 39, 41, 44, 44, 46),
                    Preco_Cafe = c(4.77, 4.67, 4.75, 4.74, 4.63, 4.56, 4.59, 4.75, 4.75,
                                   4.49,
                                   4.41, 4.32, 4.68, 4.66, 4.42, 4.71, 4.66, 4.46, 4.36, 4.47,
                                   4.43,
                                   4.4, 4.61, 4.09, 3.73, 3.89, 4.35, 3.84, 3.81, 3.79),
                    Promocao = c("Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Nao",
                                 "Nao", "Sim",
                                 "Nao", "Sim", "Nao", "Nao", "Sim", "Sim", "Nao", "Sim",
                                 "Sim",
                                 "Sim", "Nao", "Nao", "Sim", "Sim", "Sim", "Nao", "Sim",
                                 "Sim",
                                 "Sim", "Sim", "Sim"),
                    Preco_Leite = c(4.74, 4.81, 4.36, 4.29, 4.17, 4.66, 4.73, 4.11, 4.21,
                                    4.25,
                                    4.62, 4.53, 4.44, 4.19, 4.37, 4.29, 4.57, 4.21, 4.77, 4,
                                    4.31,
                                    4.34, 4.05, 4.73, 4.07, 4.75, 4, 4.15, 4.34, 4.15) )

#visualiza a media (mean) e outras estatisticas descritivas das variaveis
summary(dados)

#Visualiza desvio padrao (standard deviation) das variaveis
sd(dados$Vendas_Cafe)
sd(dados$Preco_Cafe)
sd(dados$Preco_Leite)

#Visualiza atraves de um histograma a distribuicao da variavel Preco_Cafe
hist(dados$Preco_Cafe)

# Customizando o histograma
hist(dados$Preco_Cafe,
     col = 'blue',
     main = 'Distribuicao dos Preços Praticados para o Café')

#Visualiza o histograma das tres variaveis numericas na mesma pagina
par(mfrow=c(2,2)) #Configura layout para posicionar os graficos em duas linhas e duas colunas

hist(dados$Vendas_Cafe,
     col = 'blue',
     main = 'Distribuicao das Vendas do Café')
hist(dados$Preco_Cafe,
     col = 'blue',
     main = 'Distribuicao dos Preços do Café')
hist(dados$Preco_Leite,
     col = 'blue',
     main = 'Distribuicao dos Preços do Leite')

#limpa os graficos e volta o layout para configuracao normal
dev.off() 

#Visualiza relacao entre as vendas do café o preço do café
plot(y = dados$Vendas_Cafe,
     x = dados$Preco_Cafe)

#Customiza o grafico
plot(y = dados$Vendas_Cafe,
     x = dados$Preco_Cafe,
     pch = 16,
     col = 'blue',
     xlab = 'Preço',
     ylab = 'Quantidade Vendidade',
     main = 'Relação entre o Preço e as Vendas do Café')
grid() #este comando adiciona linhas de grade ao grafico

#Colore os pontos em que havia promoção naquele dia
plot(y = dados$Vendas_Cafe,
     x = dados$Preco_Cafe,
     col =factor(dados$Promocao),
     pch = 16,
     xlab = 'Preço',
     ylab = 'Quantidade Vendidade',
     main = 'Relação entre o Preço e as Vendas do Café')

#adiciona legenda
legend(x=4.4,y=45,
       c("Promoção","Sem_Promoção"),
       col=c("red","black"),
       pch=c(16,16))
grid()


#Cria uma nova variavel informando se naquele dia vendeu acima ou abaixo da media historica
media <- mean(dados$Vendas_Cafe) #armazena a media em uma variavel
variavel <- ifelse(dados$Vendas_Cafe > media,
                   'Acima_da_media',
                   'Abaixo_da_media')

#converte nova variavel para factor
variavel <- factor(variavel) 

#grafico com a qtde abaixo e acima da media
plot(variavel) #grafico com a qtde abaixo e acima da media

#visualiza a qtde abaixo e acima da media
table(variavel)

#Gera boxplot das vendas
boxplot(dados$Vendas_Cafe)

#Gera boxplot do preco
boxplot(dados$Preco_Cafe)

#Gera boxplot comparativo das vendas quando houve promocao e de quando nao houve
boxplot(dados$Vendas_Cafe~dados$Promocao)

#Customizando o boxplot
boxplot(dados$Vendas_Cafe~dados$Promocao,
        col = 'gray',
        pch = 16,
        xlab = 'Promoção',
        ylab = 'Vendas',
        main = 'Vendas com promoção vs Vendas sem promoção')
