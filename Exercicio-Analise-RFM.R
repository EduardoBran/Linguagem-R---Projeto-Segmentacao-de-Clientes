# Praticando Análise RFM
# Segmentação de Clientes com Base em Análise RFM (Recência, Frequência e Valor Monetário)


# Configurando Diretório de Trabalho
setwd("C:/Users/Julia/Desktop/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/6.Projeto-BigDataNaPratica-Segmentacao-de-Clientes-com-Base-em-Analise-RFM")
getwd()


# Carregar pacotes necessários

library(lubridate)
library(tidyverse)      # manipula dados
library(dplyr)          # manipula dados
library(ggplot2)        # criar gráficos
library(plotly)         # criar gráficos
library(caret)          # criar modelo de aprendizado de máquina
library(readxl)         # ler planilha excel
library(rfm)            # analise rfm
library(stats)          # calculo estatítisco
library(factoextra)     # permite trabalhar com variáveis de forma diferente



# Criando e gerando dados aleatórios

set.seed(123)

dados <- data.frame(
  customer_id = rep(1:800, each = 5),
  date = sample(seq(as.Date('2020/01/01'), as.Date('2021/12/31'), by = "day"), 4000, replace = TRUE),
  price = sample(seq(10, 500, by = 1), 4000, replace = TRUE),
  quantity = sample(seq(1, 10, by = 1), 4000, replace = TRUE)
)

# Introduzir valores NA e erráticos

dados$price[1:400] <- NA
dados$price[c(800, 1200, 1600)] <- 0
dados$date[c(200, 400, 600)] <- NA



# Realizando análise exploratória

View(dados)

dim(dados)
str(dados)



# Verificando se existem dados NA

funcao_verifica_valor_ausente <- function(dataframe) {
  
  return(colSums(is.na(dataframe)))
}

funcao_verifica_valor_ausente(dados)



# Apagando linhas onde existem dados NA

funcao_remove_linhas_na <- function(dataframe) {
  
  dataframe <- na.omit(dataframe)
  
  return(dataframe)
}

dados <- funcao_remove_linhas_na(dados)

dim(dados)



# Apagando linhas onde preço igual a zero

funcao_remove_price_0 <- function(dataframe) {
  
  dataframe <- dados %>% filter(price != 0)
  
  return(dataframe)
}

dados <- funcao_remove_price_0(dados)

dim(dados)



# Criando colunas TotalPrice com valor total gasto na compra do produto

funcao_coluna_totalprice <- function(dataframe) {
  
  dataframe$total_price <- dataframe$price * dataframe$quantity
  
  return(dataframe)
}

dados <- funcao_coluna_totalprice(dados)

dim(dados)




# Colocando toda a análise exploratória em uma única função

funcao_processar_dados <- function(data){
  
  # Remove NA
  data <- na.omit(data)
  
  # Remove preço = 0
  data <- dados %>% filter(price != 0)
  
  # Cria coluna TotalPrice
  data$total_price <- data$price * data$quantity
  
  return(data)
}

dados <- funcao_processar_dados(dados)

dim(dados)


# Verificando a distribuição da variável Total Price

ggplot(dados, aes(x = total_price)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.35) +
  labs(title = 'Distribuição da Variável TotalPrice') +
  scale_x_continuous(limits = c(0, 100),  # Define o intervalo dos valores do eixo x
                     breaks = seq(0, 100, by = 10),  # Define os rótulos dos intervalos no eixo x
                     labels = function(x) paste0("$", x))  # Define os rótulos dos valores do eixo x com o símbolo de dólar





# Verificando número de clientes

# desta forma retorna todas as linhas de Customer ID e assim não saberemos ao certo se são todos os clientes

length(dados$customer_id)

# Para isso usamos o unique

num_customers <- length(unique(dados$customer_id))

cat("Número de clientes únicos:", num_customers)




# Verificando quando cada cliente gastou

total_gasto_cliente <- dados %>%
  group_by(customer_id) %>%
  summarise(total = sum(total_price))

View(total_gasto_cliente)

# Criação do gráfico de barras

ggplot(total_gasto_cliente, aes(x = customer_id, y = total, fill = customer_id)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Gasto por Cliente", x = "Cliente ID", y = "Total Gasto")





# Com base nos dados precisamos tomar a decisão para escolher o valor da Recência, que é quando o cliente fez
# a última compra (pode ser mês, dia, hora)


# Analisando a maior data (última data que temos de uma compra)
max(dados$date)


# Criando uma data customizada (Janeiro de 2022) que será o nosso valor da recência com base na maior data

data_valor_recencia <- as.Date.character("02/01/2022", "%d/%m/%Y")



# Criando função para calcular Recência, Frequência e Valor Monetário

funcao_calcula_rfm <- function(data) {
  
  r <- dados %>%
    group_by(customer_id) %>%
    summarise(Recency = as.numeric(data_valor_recencia - max(date)),
              Frequency = n(),
              Monetary = sum(total_price),
              Primeira_Compra = min(date),
              Ultima_Compra = max(date))
  
  return(r)
}

dados_valores_rfm <- funcao_calcula_rfm(dados)

View(dados_valores_rfm)



# Aplicando machine learning com clusterização com kmneans. 
# Clusterização é uma técnica de aprendizado não supervisionado.


# Função para a segmentação de clientes com base nos valores RFM

set.seed(123)

funcao_segmenta_cliente <- function(data) {
  
  # cria uma lista
  resultados <- list()
  
  # filtra colunas
  dados_rfm <- select(data, c('Recency', 'Frequency', 'Monetary'))
  
  # cria o modelo kmeans
  modelo_kmeans <- kmeans(dados_rfm, center = 5, iter.max = 50)
  
  # plot do modelo
  resultados$plot <- fviz_cluster(modelo_kmeans,
                                  data = dados_rfm,
                                  geom = c('point'),
                                  ellipse.type = 'euclid')
  
  # organiza os dados
  dados_rfm$Customer_Id <- data$customer_id
  dados_rfm$Clusters <- modelo_kmeans$cluster
  
  resultados$data <- dados_rfm
  
  
  return(resultados)
}

# Exibindo resultado em formato de gráfico

grafico <- funcao_segmenta_cliente(dados_valores_rfm)[1]
grafico

# Exibindo resultado em dataframe

tabela_resultados_rfm <- funcao_segmenta_cliente(dados_valores_rfm)[2]

tabela_resultados_rfm <- as.data.frame(tabela_resultados_rfm)

View(tabela_resultados_rfm)



# Analisando / filtrando os dados acima

# Analisando valor médio da recência por grupo

# Quanto menor o valor de data.Recency, mais recente foi a última compra do cliente, o que significa que demoram menos tempo para
# fazer uma nova compra após a última.
# E quanto maior o valor de data.Recency, mais tempo se passou desde a última compra, o que sugere que o cliente compra com menos
# frequência. 

valor_recencia <- tabela_resultados_rfm %>%
  group_by(data.Clusters) %>%
  summarise(Media.Recencia = mean(data.Recency))

View(valor_recencia)

# Plot (gráfico barras)
ggplot(valor_recencia, aes(x = data.Clusters, y = Media.Recencia)) +
  geom_bar(stat = "identity") +
  labs(x = "Grupo", y = "Média de Recência", title = "Média de Recência por Grupo")



# Analisando valor médio de frequência por grupo

# E quanto maior o valor de data.Frequency, sigfnica que o grupo compra com mais frequência

valor_frequencia <- tabela_resultados_rfm %>%
  group_by(data.Clusters) %>%
  summarise(Media.Frequencia = mean(data.Frequency))

View(valor_frequencia)



# Analisando valor médio Monetário por grupo

# E quanto maior o valor de data.Monetary, sigfnica que o grupo gasta mais em suas compras

valor_monetario <- tabela_resultados_rfm %>%
  group_by(data.Clusters) %>%
  summarise(Media.Monetario = mean(data.Monetary))

# Analisando valor total Monetário por grupo

valor_monetario_total <- tabela_resultados_rfm %>%
  group_by(data.Clusters) %>%
  summarise(Total.Monetario = sum(data.Monetary))




