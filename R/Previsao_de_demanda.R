# Previsão de Demanda utilizando o R
# O excel há um limite de linhas e colunas.
# Dependendo do conjunto de dados e sua complexidade o R é eficiente.


library(ggplot2)

ano_1 <- c(2000,3000,3000,3000,4000,6000,7000,6000,10000,12000,14000,8000)
ano_2 <- c(3000,4000,1000,5000,8000,3000,8000,12000,12000,16000,10000,3000)
ano_3 <- c(2000,5000,5000,3000,4000,6000,7000,10000,15000,15000,18000,8000)
ano_4 <- c(5000,4000,4000,2000,5000,7000,11000,14000,16000,16000,20000,12000)
ano_5 <- c(6000,2000,3000,2000,7000,6000,8000,10000,20000,20000,22000,8000)

meses <- c("Janeiro","Fevereiro","Março","Abril",
                      "Maio","Junho","Julho","Agosto",
                      "Setembro","Outubro","Novembro",
                      "Dezembro")

meses_n <- c(1,2,3,4,5,6,7,8,9,10,11,12)

dados <- data.frame(ano_1,ano_2,ano_3,ano_4,ano_5)
row.names(dados) <-  meses

print(dados)

# Visualização dos pontos:

ggplot(dados, aes(y = ano_1 + ano_2 + ano_3 + ano_4
                  + ano_5, x = meses_n)) + geom_point()

# Realização do ajuste do modelo, armazenado na variavel "modelo"
# Regressão linear (lm) o ~ é 'referente a em função de'

modelo <- lm(data = dados, formula = meses_n ~ ano_1 + ano_2 +
               ano_3 + ano_4 + ano_5)
# a função abaixo ira consultar a estimativa dos parametros esta ok

modelo$coefficients

summary(modelo)

ggplot(dados, aes(x = meses_n, y = ano_1 + ano_2 + ano_3 + ano_4
                  + ano_5)) + geom_point() +
                  geom_smooth(method = lm, se = FALSE)

# Normalização dos residuos e interdependencia
# Dependendo do vídeo mude o parametro do par 

par(mfrow = c(2,2))
plot(modelo, which = c(1:4), pch = 20)