library(ggplot2)
library(dplyr)
library(DAAG)
library(tidyverse)
library(recipes)
library(caret)
library(pROC)
library(readxl)

str(Mortalidade_Geral_2020)
str(Mortalidade_Geral_2000)

# Criar um vetor com valores 1 e 2 representando o sexo
SEXO <- c(1, 2, 2, 1, 2)

# Converter os valores para "male" e "female"
SEXO_convertido <- ifelse(SEXO == 1, "male", "female")

# Exibir o vetor convertido
print(SEXO_convertido)


#Preprocessamento
Mortalidade_Geral_2020$DTOBITO<-as.factor(Mortalidade_Geral_2020$DTOBITO)
Mortalidade_Geral_2020$IDADE<-as.factor(Mortalidade_Geral_2020$IDADE)
Mortalidade_Geral_2020$CODMUNOCOR<-as.factor(Mortalidade_Geral_2020$CODMUNOCOR)
Mortalidade_Geral_2020$SEXO<-as.factor(Mortalidade_Geral_2020$SEXO)

Mortalidade_Geral_2000$DTOBITO<-as.factor(Mortalidade_Geral_2000$DTOBITO)
Mortalidade_Geral_2000$IDADE<-as.factor(Mortalidade_Geral_2020$IDADE)
Mortalidade_Geral_200$CODMUNOCOR<-as.factor(Mortalidade_Geral_2020$CODMUNOCOR)
Mortalidade_Geral_2000$SEXO<-as.factor(Mortalidade_Geral_2020$SEXO)

str(Mortalidade_Geral_2020)
str(Mortalidade_Geral_2000)



#Análise exploratória
Mortalidade_Geral_2000%>%
  count(SEXO)

# Fazer a contagem de sexo
contagem_sexo <- Mortalidade_Geral_2000 %>%
  group_by(SEXO) %>%
  summarise(Contagem = n())

# Criar o gráfico de barras
grafico <- ggplot(data = contagem_sexo, aes(x = SEXO, y = Contagem)) +
  geom_bar(stat = "identity") +
  labs(x = "Sexo", y = "Contagem", title = "Contagem de Mortes por Sexo em 2000")

# Exibir o gráfico
print(grafico)


#Análise exploratória
Mortalidade_Geral_2020%>%
  count(SEXO)

# Fazer a contagem de sexo
contagem_sexo <- Mortalidade_Geral_2020 %>%
  group_by(SEXO) %>%
  summarise(Contagem = n())

# Criar o gráfico de barras
grafico <- ggplot(data = contagem_sexo, aes(x = SEXO, y = Contagem)) +
  geom_bar(stat = "identity") +
  labs(x = "Sexo", y = "Contagem", title = "Contagem de Mortes por Sexo em 2020")

# Exibir o gráfico
print(grafico)




  # Criar um dataframe de exemplo com os dados de 2000 e 2020
  Mortalidade_Geral_2000 <- data.frame(
    Ano = rep("2000", times = 4),
    Sexo = c("M", "F"),
    Contagem = c(150, 180, 120, 160)
  )

Mortalidade_Geral_2020 <- data.frame(
  Ano = rep("2020", times = 4),
  Sexo = c("M", "F"),
  Contagem = c(180, 200, 150, 190)
)

dados_comparativo <- rbind(Mortalidade_Geral_2000, Mortalidade_Geral_2020)

# Criar o gráfico de barras comparativo
grafico <- ggplot(data = dados_comparativo, aes(x = Ano, y = Contagem, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Ano", y = "Contagem", title = "Comparativo de Mortalidade por Sexo (2000 vs 2020)")

# Exibir o gráfico
print(grafico)


# Criar um vetor de exemplo com os dados de idade
IDADE <- c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95)

# Criar o histograma
histograma <- ggplot(data = NULL, aes(x = IDADE)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(x = "Idade", y = "Contagem", title = "Distribuição de Idade")

# Exibir o histograma
print(histograma)




# Calcular a média de idade e o índice de mortalidade para homens e mulheres em 2000 e 2020
media_idade <- rbind(
  Mortalidade_Geral_2000 %>%
    group_by(SEXO) %>%
    summarise(Media_Idade = mean(IDADE),
              Ano = "2000"),
  Mortalidade_Geral_2020 %>%
    group_by(SEXO) %>%
    summarise(Media_Idade = mean(IDADE),
              Ano = "2020")
)

# Criar o gráfico de pontos comparativo
grafico_pontos <- ggplot(data = media_idade, aes(x = Ano, y = Media_Idade, color = SEXO)) +
  geom_point() +
  labs(x = "Ano", y = "Média de Idade", title = "Comparativo de Média de Idade por Sexo")

# Exibir o gráfico de pontos
print(grafico_pontos)




library(ggplot2)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2020$DTOBITO <- as.Date(Mortalidade_Geral_2020$DTOBITO, format = "%Y-%m-%d")

# Contar o número de mortes por data de óbito
resumo_mortalidade <- Mortalidade_Geral_2020 %>%
  group_by(DTOBITO) %>%
  summarise(Numero_Mortes = n())

# Gerar o gráfico de linhas
grafico <- ggplot(data = resumo_mortalidade, aes(x = DTOBITO, y = Numero_Mortes)) +
  geom_line() +
  labs(x = "Data de Óbito", y = "Número de Mortes", title = "Número de Mortes por Data de Óbito")

# Exibir o gráfico
print(grafico)


# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Contar o número de mortes por data de óbito
resumo_mortalidade <- Mortalidade_Geral_2000 %>%
  group_by(DTOBITO) %>%
  summarise(Numero_Mortes = n())

# Gerar o gráfico de linhas
grafico <- ggplot(data = resumo_mortalidade, aes(x = DTOBITO, y = Numero_Mortes)) +
  geom_line() +
  labs(x = "Data de Óbito", y = "Número de Mortes", title = "Número de Mortes por Data de Óbito")

# Exibir o gráfico
print(grafico)





library(ggplot2)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Filtrar as pessoas entre 0 e 50 anos
dados_filtrados <- Mortalidade_Geral_2000 %>%
  filter(IDADE >= 0 & IDADE <= 50)

# Contar o número de mortes por data de óbito
resumo_mortalidade <- dados_filtrados %>%
  group_by(DTOBITO) %>%
  summarise(Numero_Mortes = n())

# Gerar o gráfico de linhas
grafico <- ggplot(data = resumo_mortalidade, aes(x = DTOBITO, y = Numero_Mortes)) +
  geom_line() +
  labs(x = "Data de Óbito", y = "Número de Mortes", title = "Número de Mortes por Data de Óbito (Idade entre 0 e 50 anos)")

# Exibir o gráfico
print(grafico)


# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2020$DTOBITO <- as.Date(Mortalidade_Geral_2020$DTOBITO, format = "%Y-%m-%d")

# Filtrar as pessoas entre 0 e 50 anos
dados_filtrados <- Mortalidade_Geral_2020 %>%
  filter(IDADE >= 0 & IDADE <= 50)

# Contar o número de mortes por data de óbito
resumo_mortalidade <- dados_filtrados %>%
  group_by(DTOBITO) %>%
  summarise(Numero_Mortes = n())

# Gerar o gráfico de linhas
grafico <- ggplot(data = resumo_mortalidade, aes(x = DTOBITO, y = Numero_Mortes)) +
  geom_line() +
  labs(x = "Data de Óbito", y = "Número de Mortes", title = "Número de Mortes por Data de Óbito (Idade entre 0 e 50 anos)")

# Exibir o gráfico
print(grafico)





library(ggplot2)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Contar o número de mortes por data de óbito e sexo
resumo_mortalidade <- Mortalidade_Geral_2000 %>%
  group_by(DTOBITO, SEXO) %>%
  summarise(Numero_Mortes = n())

# Identificar a data com o maior número de mortes em um único dia para cada sexo
data_max_mortes <- resumo_mortalidade %>%
  group_by(SEXO) %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  ungroup()

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_mortalidade, aes(x = DTOBITO, y = Numero_Mortes, fill = factor(SEXO))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Data de Óbito", y = "Número de Mortes", title = "Número de Mortes por Data de Óbito (por Sexo)") +
  geom_text(data = data_max_mortes, aes(label = Numero_Mortes), vjust = -0.5, position = position_dodge(width = 1))

# Personalizar as cores dos grupos de sexo
grafico <- grafico + scale_fill_manual(values = c("blue", "red", "green"), labels = c("Masculino", "Feminino"))

# Exibir o gráfico
print(grafico)





library(ggplot2)
library(dplyr)
library(lubridate)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Extrair o mês da coluna "DTOBITO"
dados_mortalidade <- Mortalidade_Geral_2000 %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes por mês
resumo_mortalidade <- dados_mortalidade %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes
mes_max_mortes <- resumo_mortalidade %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_mortalidade, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)


# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2020$DTOBITO <- as.Date(Mortalidade_Geral_2020$DTOBITO, format = "%Y-%m-%d")

# Extrair o mês da coluna "DTOBITO"
dados_mortalidade <- Mortalidade_Geral_2020 %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes por mês
resumo_mortalidade <- dados_mortalidade %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes
mes_max_mortes <- resumo_mortalidade %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_mortalidade, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)





library(ggplot2)
library(dplyr)
library(lubridate)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2020$DTOBITO <- as.Date(Mortalidade_Geral_2020$DTOBITO, format = "%Y-%m-%d")

# Filtrar as mortes de crianças até 11 anos
dados_criancas <- Mortalidade_Geral_2020 %>%
  filter(IDADE <= 11)

# Extrair o mês da coluna "DTOBITO"
dados_criancas <- dados_criancas %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes de crianças por mês
resumo_criancas <- dados_criancas %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes de crianças
mes_max_mortes <- resumo_criancas %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_criancas, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes de Crianças até 11 anos por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)





# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Filtrar as mortes de crianças até 11 anos
dados_criancas <- Mortalidade_Geral_2000 %>%
  filter(IDADE <= 11)

# Extrair o mês da coluna "DTOBITO"
dados_criancas <- dados_criancas %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes de crianças por mês
resumo_criancas <- dados_criancas %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes de crianças
mes_max_mortes <- resumo_criancas %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_criancas, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes de Crianças até 11 anos por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)



library(ggplot2)
library(dplyr)
library(lubridate)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Filtrar as mortes de crianças até 11 anos do sexo masculino
dados_criancas_masculinas <- Mortalidade_Geral_2000 %>%
  filter(IDADE <= 11, SEXO == 1)

# Extrair o mês da coluna "DTOBITO"
dados_criancas_masculinas <- dados_criancas_masculinas %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes de crianças masculinas por mês
resumo_criancas_masculinas <- dados_criancas_masculinas %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes de crianças masculinas
mes_max_mortes <- resumo_criancas_masculinas %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_criancas_masculinas, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes de Crianças Masculinas até 11 anos por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)




# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2000$DTOBITO <- as.Date(Mortalidade_Geral_2000$DTOBITO, format = "%Y-%m-%d")

# Filtrar as mortes de crianças até 11 anos do sexo feminino
dados_criancas_masculinas <- Mortalidade_Geral_2000 %>%
  filter(IDADE <= 11, SEXO == 2)

# Extrair o mês da coluna "DTOBITO"
dados_criancas_femininas <- dados_criancas_femininas %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes de crianças femininas por mês
resumo_criancas_femininas <- dados_criancas_femininas %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes de crianças feminino
mes_max_mortes <- resumo_criancas_femininas %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_criancas_femininas, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes de Crianças Feminino até 11 anos por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)



library(ggplot2)
library(dplyr)
library(lubridate)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2020$DTOBITO <- as.Date(Mortalidade_Geral_2020$DTOBITO, format = "%Y-%m-%d")

# Filtrar as mortes de crianças até 11 anos do sexo feminino
dados_criancas_femininas <- Mortalidade_Geral_2020 %>%
  filter(IDADE <= 11, SEXO == 2)

# Extrair o mês da coluna "DTOBITO"
dados_criancas_femininas <- dados_criancas_femininas %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes de crianças femininas por mês
resumo_criancas_femininas <- dados_criancas_femininas %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes de crianças femininas
mes_max_mortes <- resumo_criancas_femininas %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_criancas_femininas, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes de Crianças Femininas até 11 anos por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)




library(ggplot2)
library(dplyr)
library(lubridate)

# Converter a coluna "DTOBITO" para o formato de data
Mortalidade_Geral_2020$DTOBITO <- as.Date(Mortalidade_Geral_2020$DTOBITO, format = "%Y-%m-%d")

# Filtrar as mortes de crianças até 11 anos do sexo masculino
dados_criancas_masculinas <- Mortalidade_Geral_2020 %>%
  filter(IDADE <= 11, SEXO == 1)

# Extrair o mês da coluna "DTOBITO"
dados_criancas_masculinas <- dados_criancas_masculinas %>%
  mutate(Mes = month(DTOBITO))

# Contar o número de mortes de crianças masculinas por mês
resumo_criancas_masculinas <- dados_criancas_masculinas %>%
  group_by(Mes) %>%
  summarise(Numero_Mortes = n())

# Identificar o mês com o maior número de mortes de crianças masculinas
mes_max_mortes <- resumo_criancas_masculinas %>%
  filter(Numero_Mortes == max(Numero_Mortes)) %>%
  pull(Mes)

# Gerar o gráfico de barras
grafico <- ggplot(data = resumo_criancas_masculinas, aes(x = factor(Mes), y = Numero_Mortes)) +
  geom_bar(stat = "identity") +
  labs(x = "Mês", y = "Número de Mortes", title = "Número de Mortes de Crianças Masculinas até 11 anos por Mês") +
  geom_text(aes(label = ifelse(Mes == mes_max_mortes, Numero_Mortes, "")), vjust = -0.5)

# Exibir o gráfico
print(grafico)
