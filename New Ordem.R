library(dplyr)
library(glue)
library(geobr)

str(Mortalidade_Geral_2000)
str(municipios)

#excluir os dados missing da planilha
dados_completos <- na.omit(Mortalidade_Geral_2000)

#exibo a planilha no console
dados_completos

#Substituir o 1 - M e 2 - F
Mortalidade_Geral_2000$SEXO <- ifelse(Mortalidade_Geral_2000$SEXO == 1, "M", "F")

#Visualizar a planilha
View(Mortalidade_Geral_2000)

#Visualizar os dados no console
print(Mortalidade_Geral_2000)

#Formatando a Data do Obito com apenas o Dia e o Mês 
Mortalidade_Geral_2000$DTOBITO <- format(as.Date(Mortalidade_Geral_2000$DTOBITO), format = "%d/%m")

#Vizualizando a Planilha com a coluna alterada
View(Mortalidade_Geral_2000)

# Definir o intervalo de idade desejado
idade_minima <- 18
idade_maxima <- 65

# Adicionar idades aleatórias à coluna "IDADE"
Mortalidade_Geral_2000 <- Mortalidade_Geral_2000 %>% mutate(IDADE = sample(idade_minima:idade_maxima, nrow(Mortalidade_Geral_2000), replace = TRUE))

# Verificar o dataframe atualizado
View(Mortalidade_Geral_2000) #verificar na planilha 


# Excluir a coluna usando a função select
municipios <- select(municipios, -uf_code, -microregion, -rgint, -rgi, -osm_relation_id, -wikidata_id, -is_capital, -wikipedia_pt, -lon, -lat, -no_accents, -slug_name)
municipios <- select(municipios, -pop_21, -alternative_names)
municipios <- select(municipios, -uf, -mesoregion)
View(municipios)

#Fiz a junção das planilhas
Mortalidade_Geral_2000 <- merge(Mortalidade_Geral_2000, municipios, by = "CODMUNIC")
View(Mortalidade_Geral_2000)

#Apaguei a coluna Municipio
Mortalidade_Geral_2000 <- select(Mortalidade_Geral_2000, -municipio)

#Visualizei a alteração feita
View(Mortalidade_Geral_2000)



library(dplyr)

# Vetor de números
numeros <- c(1, 5, 3, 4, 9, 2, "NA")

# Vetor de palavras correspondentes
palavras <- c("Hospital", "Outros", "Domicilio", "Via Publica", "Ignorado", "Outros Estabelecimento", "N/A")

# Atualizar a coluna 'LOCOCOR' com as palavras correspondentes
Mortalidade_Geral_2000 <- Mortalidade_Geral_2000 %>% mutate(LOCOCOR = palavras[match(LOCOCOR, numeros)])

View(Mortalidade_Geral_2000)

unique(Mortalidade_Geral_2000$LOCOCOR)


library(ggplot2)

# Agrupar os dados por sexo e contar o número de ocorrências em cada grupo
dados_por_sexo <- Mortalidade_Geral_2000 %>% 
  group_by(SEXO) %>% 
  summarise(Numero_Mortes = n())

# Criar o gráfico de barras
grafico <- ggplot(data = dados_por_sexo, aes(x = SEXO, y = Numero_Mortes, fill = SEXO)) +
  geom_bar(stat = "identity") +
  labs(x = "Sexo", y = "Número de Mortes", title = "Número de Mortes por Sexo em 2000")

# Exibir o gráfico
print(grafico)


library(ggplot2)

# Agrupar os dados por município e sexo, e contar o número de ocorrências em cada grupo
dados_por_municipio_sexo <- Mortalidade_Geral_2000 %>% 
  group_by(CODMUNIC, SEXO) %>% 
  summarise(Numero_Mortes = n())

# Criar o gráfico de barras
grafico <- ggplot(data = dados_por_municipio_sexo, aes(x = CODMUNIC, y = Numero_Mortes, fill = SEXO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Município", y = "Número de Mortes", title = "Número de Mortes por Sexo em 2000 por Município") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exibir o gráfico
print(grafico)


library(dplyr)
library(ggplot2)

# Importe os dados para o ambiente de trabalho em R, substituindo "seus_dados" pelo nome do seu conjunto de dados
dados <- Mortalidade_Geral_2000

# Calcule o número de óbitos por "LOCOCOR" e sexo
contagem <- dados %>% count(LOCOCOR, SEXO)

# Ordene os dados pela contagem de óbitos em ordem decrescente para cada sexo
contagem <- contagem %>% arrange(desc(n))

# Crie um gráfico de barras para visualizar os resultados
grafico <- ggplot(contagem, aes(x = LOCOCOR, y = n, fill = SEXO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "LOCOCOR", y = "Número de Óbitos", fill = "Sexo") +
  ggtitle("Local com Maior Número de Óbitos por Sexo em 2000") +
  theme_minimal()

# Visualize o gráfico
print(grafico)



library(ggplot2)

# Criar um data frame com os dados de exemplo
dados <- data.frame(
  sexo = c("Masculino", "Feminino", "Masculino", "Feminino", "Masculino"),
  localizacao = c("Hospitais", "Vias Públicas", "Outros Estabelecimentos", "Domicílio", "Ignorados")
)

# Criar o gráfico de barras empilhadas
ggplot(data = dados, aes(x = sexo, fill = localizacao)) +
  geom_bar(position = "stack") +
  labs(title = "Distribuição de Sexo por Localização do Óbito",
       x = "Sexo",
       y = "Contagem") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"))



# Suponha que você tenha um conjunto de dados chamado "dados" com as informações necessárias
# Certifique-se de que as colunas "DataObito", "Sexo", "Idade", "Localização do Obito" e "Municípios" estejam presentes no conjunto de dados

# Converta a variável "DataObito" para um formato numérico adequado
Mortalidade_Geral_2000$DTOBITO <- as.numeric(Mortalidade_Geral_2000$DTOBITO)

# Converta a variável "Sexo" para um formato numérico adequado
Mortalidade_Geral_2000$SEXO <- as.numeric(Mortalidade_Geral_2000$SEXO)

# Converta a variável "Idade" para um formato numérico adequado
Mortalidade_Geral_2000$IDADE <- as.numeric(Mortalidade_Geral_2000$IDADE)

# Converta a variável "Localização do Obito" para um formato numérico adequado
Mortalidade_Geral_2000$LOCOCOR <- as.numeric(Mortalidade_Geral_2000$LOCOCOR)

# Converta a variável "Municípios" para um formato numérico adequado
#Mortalidade_Geral_2000$ <- as.numeric(dados$Municipios)

# Realize a regressão linear
modelo <- lm(DTOBITO ~ SEXO + IDADE + LOCOCOR, data = Mortalidade_Geral_2000)

# Exiba os resultados da regressão
summary(modelo)