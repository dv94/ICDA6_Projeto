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

