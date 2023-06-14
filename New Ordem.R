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
