library(dplyr)
library(glue)
library(geobr)

str(Mortalidade_Geral_2020)
str(municipios)

#excluir os dados missing da planilha
dados_completos <- na.omit(Mortalidade_Geral_2020)

#exibo a planilha no console
dados_completos

#Substituir o 1 - M e 2 - F
Mortalidade_Geral_2020$SEXO <- ifelse(Mortalidade_Geral_2020$SEXO == 1, "M", "F")

#Visualizar a planilha
View(Mortalidade_Geral_2020)

#Visualizar os dados no console
print(Mortalidade_Geral_2020)

#Formatando a Data do Obito com apenas o Dia e o Mês 
Mortalidade_Geral_2020$DTOBITO <- format(as.Date(Mortalidade_Geral_2020$DTOBITO), format = "%d/%m")

#Vizualizando a Planilha com a coluna alterada
View(Mortalidade_Geral_2020)


# Definir o intervalo de idade desejado
idade_minima <- 18
idade_maxima <- 65

# Adicionar idades aleatórias à coluna "IDADE"
Mortalidade_Geral_2020 <- Mortalidade_Geral_2020 %>% mutate(IDADE = sample(idade_minima:idade_maxima, nrow(Mortalidade_Geral_2020), replace = TRUE))

# Verificar o dataframe atualizado
View(Mortalidade_Geral_2020) #verificar na planilha 

#---------------------
#Fiz a junção das planilhas
Mortalidade_Geral_2020 <- merge(Mortalidade_Geral_2020, municipios, by = "CODMUNIC")
View(Mortalidade_Geral_2020)

#Apaguei a coluna Municipio
Mortalidade_Geral_2020 <- select(Mortalidade_Geral_2020, -municipio)

#Visualizei a alteração feita
View(Mortalidade_Geral_2020)
#---------------------




library(dplyr)

# Vetor de números
numeros <- c(1, 5, 3, 4, 9, 2, "NA")

# Vetor de palavras correspondentes
palavras <- c("Hospital", "Outros", "Domicilio", "Via Publica", "Ignorado", "Outros Estabelecimento", "N/A")

# Atualizar a coluna 'LOCOCOR' com as palavras correspondentes
Mortalidade_Geral_2020 <- Mortalidade_Geral_2020 %>% mutate(LOCOCOR = palavras[match(LOCOCOR, numeros)])

View(Mortalidade_Geral_2020)

unique(Mortalidade_Geral_2020$LOCOCOR)


library(ggplot2)

# Agrupar os dados por sexo e contar o número de ocorrências em cada grupo
dados_por_sexo <- Mortalidade_Geral_2020 %>% 
  group_by(SEXO) %>% 
  summarise(Numero_Mortes = n())

# Criar o gráfico de barras
grafico <- ggplot(data = dados_por_sexo, aes(x = SEXO, y = Numero_Mortes, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Sexo", y = "Número de Mortes", title = "Número de Mortes por Sexo em 2020")

# Exibir o gráfico
print(grafico)


library(ggplot2)

# Agrupar os dados por município e sexo, e contar o número de ocorrências em cada grupo
dados_por_municipio_sexo <- Mortalidade_Geral_2020 %>% 
  group_by(CODMUNIC, SEXO) %>% 
  summarise(Numero_Mortes = n())

# Criar o gráfico de barras
grafico <- ggplot(data = dados_por_municipio_sexo, aes(x = CODMUNIC, y = Numero_Mortes, fill = SEXO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Município", y = "Número de Mortes", title = "Número de Mortes por Sexo em 2020 por Município") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exibir o gráfico
print(grafico)


library(dplyr)
library(ggplot2)

# Importe os dados para o ambiente de trabalho em R, substituindo "seus_dados" pelo nome do seu conjunto de dados
dados <- Mortalidade_Geral_2020

# Calcule o número de óbitos por "LOCOCOR" e sexo
contagem <- dados %>% count(LOCOCOR, SEXO)

# Ordene os dados pela contagem de óbitos em ordem decrescente para cada sexo
contagem <- contagem %>% arrange(desc(n))

# Crie um gráfico de barras para visualizar os resultados
grafico <- ggplot(contagem, aes(x = LOCOCOR, y = n, fill = SEXO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "LOCOCOR", y = "Número de Óbitos", fill = "Sexo") +
  ggtitle("Local com Maior Número de Óbitos por Sexo em 2020") +
  theme_minimal()

# Visualize o gráfico
print(grafico)


library(ggplot2)

# Importe os dados para o ambiente de trabalho em R, substituindo "seus_dados" pelo nome do seu conjunto de dados
dados <- Mortalidade_Geral_2020
dados2 <- Mortalidade_Geral_2000

# Crie um histograma para cada conjunto de dados
grafico_2000 <- ggplot(dados2, aes(x = IDADE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(x = "Idade", y = "Contagem", title = "Distribuição de Idades dos Óbitos em 2000") +
  theme_minimal()

grafico_2020 <- ggplot(dados, aes(x = IDADE)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(x = "Idade", y = "Contagem", title = "Distribuição de Idades dos Óbitos em 2020") +
  theme_minimal()

# Exiba os gráficos lado a lado ou em painéis separados
# Opção 1: Lado a lado
library(gridExtra)
grid.arrange(grafico_2000, grafico_2020, ncol = 2)

# Opção 2: Painéis separados
library(cowplot)
plot_grid(grafico_2000, grafico_2020, ncol = 1)


# Carregar a biblioteca necessária
library(ggplot2)

# Carregar os dados do projeto (substitua "seus_dados" pelo nome do conjunto de dados)
dados <- Mortalidade_Geral_2020

# Gerar o gráfico
ggplot(dados, aes(x = IDADE, y = DTOBITO)) +
  geom_point() +
  labs(x = "Idade", y = "Número de Óbitos", title = "Número de Óbitos por Idade") +
  theme_minimal()




# Carregar as bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(caret)

# Carregar os dados do projeto (substitua "seus_dados" pelo nome do conjunto de dados)
dados <- Mortalidade_Geral_2020

# Remover valores ausentes no conjunto de dados
dados <- dados[complete.cases(dados$DTOBITO), ]

# Converter variáveis categóricas em variáveis dummy
dados <- dados %>%
  mutate(SEXO = as.factor(SEXO),
         LOCOCOR = as.factor(LOCOCOR))

# Divisão dos dados em conjunto de treinamento e teste (70% treinamento, 30% teste)
set.seed(123)
indice_treinamento <- createDataPartition(dados$DTOBITO, p = 0.7, list = FALSE)
dados_treinamento <- dados[indice_treinamento, ]
dados_teste <- dados[-indice_treinamento, ]

# Remover valores ausentes no conjunto de treinamento
dados_treinamento <- dados_treinamento[complete.cases(dados_treinamento$DTOBITO), ]

# Construção do modelo de regressão linear
modelo <- lm(DTOBITO ~ IDADE + SEXO + LOCOCOR, data = dados_treinamento)

# Avaliação do modelo usando o conjunto de teste
predicoes <- predict(modelo, newdata = dados_teste)
r2 <- R2(predicoes, dados_teste$DTOBITO)
rmse <- RMSE(predicoes, dados_teste$DTOBITO)
mae <- MAE(predicoes, dados_teste$DTOBITO)

# Visualização dos resultados
ggplot() +
  geom_point(data = dados_teste, aes(x = IDADE, y = DTOBITO), color = "blue") +
  geom_line(data = dados_teste, aes(x = IDADE, y = predicoes), color = "red") +
  labs(x = "Idade", y = "Número de Óbitos", title = "Regressão Linear - Previsão de Óbitos") +
  theme_minimal()

# Imprimir métricas de avaliação
cat("Coeficiente de Determinação (R²):", r2, "\n")
cat("Erro Médio Quadrático (RMSE):", rmse, "\n")
cat("Erro Absoluto Médio (MAE):", mae, "\n")




# Carregar a biblioteca necessária
library(ggplot2)

# Carregar os dados do projeto (substitua "seus_dados" pelo nome do conjunto de dados)
dados <- Mortalidade_Geral_2020

# Verificar a estrutura dos dados
str(Mortalidade_Geral_2020)

# Realizar o teste de regressão linear
modelo <- lm(DTOBITO ~ IDADE + SEXO + LOCOCOR, data = Mortalidade_Geral_2020)

# Visualizar o resumo do modelo
summary(modelo)

