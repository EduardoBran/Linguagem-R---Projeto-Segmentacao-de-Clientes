# Big Data na Prática 4
# Segmentação de Clientes com Base em Análise RFM (Recência, Frequência e Valor Monetário)


# Configurando Diretório de Trabalho
setwd("C:/Users/Julia/Desktop/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/6.Projeto-BigDataNaPratica-Segmentacao-de-Clientes-com-Base-em-Analise-RFM")
getwd()



# Neste Big Data na Pratica vamos trabalhar com Machine Learning para segmentação de clientes (processo de dividir um mercado de
# consumidores em grupos distintos, com características, necessidades e comportamentos semelhantes.)
# Embora a segmentação possa ser feita com atributos dos clientes (como idade, renda mensar ou estado civil), vamos primeiro aplicar
# a Análise RFM (Recência, Frquência e Valor Monetário) e a partir desta análise segmentar e agrupar os clientes por similaridade,
# ajudando assim a área de Marketing ou Vendas a personalizar a experiência de compra dos clientes.

# Análise RFM - é uma técnica de segmentação de clientes que é amplamente utilizada na área de marketing e vendas e tem como base
#               três variáveis: Recência (com que frequência o cliente fez uma compra?), Frequência (com que frequência eles compram) e
#               Valor monetário (quantos eles gastam?) a fom de prever quais clientes têm maior probabilidade de realizar novas compras.

# O modelo RFM é baseado em três fatores quantitativos:

# - Recência: última vez que um cliente fez uma compra. Indica hâ quanto tempo o cliente executou uma ação desejada (ex: uma compra)
#
# - Frequência: com que frequência um cliente faz uma compra. A frequência indica quantas vezes o cleinte repetiu essa ação em um
#               determinado período de tempo.
#
# - Valor Monetário: quanto dinheiro um cliente gasta em compras.

# A Análise RFM classifica numericamente um cliente em cada uma dessas três categorias, geralmente em uma escalada de 1 a 5.

# De acordo com essas métricas, é possível segmentar os clientes em grupos para entender quais deles comprar muitas coisas com
# frequência, os que compram poucas coisa, mas frequentemente, e que não comprar nada há muito tempo.



# Neste Projeto após o carregamento dos dados iremos fazer:
#
# - organizar os dados
# - aplicar engenharia de atributos para criar uma nova variável
# - aplicar Análise RFM (extrair valores de Recência, Frequência e Valor Monetário)
# - usar estes dados para treinar o modelo de aprendizagem de máquina que irá encontrar os clientes por similaridade
# - por fim dividir os clientes em 5 grupos



# Instalando e importando pacotes

library(tidyverse)      # manipula dados
library(dplyr)          # manipula dados
library(ggplot2)        # criar gráficos
library(plotly)         # criar gráficos
library(caret)          # criar modelo de aprendizado de máquina
library(readxl)         # ler planilha excel
library(rfm)            # analise rfm
library(stats)          # calculo estatítisco
library(factoextra)     # permite trabalhar com variáveis de forma diferente


# Função para carregar os dados da planilha Excel (este arquivo excel possui 2 planilhas)
# foi necessário adicionar um range para conseguir fazer a leitura dos dados (ler da celular A1 até H99999)

carrega_dados <- function() {
  
  setwd("C:/Users/Julia/Desktop/CienciaDeDados/1.Big-Data-Analytics-com-R-e-Microsoft-Azure-Machine-Learning/6.Projeto-BigDataNaPratica-Segmentacao-de-Clientes-com-Base-em-Analise-RFM")
  
  intervalo1 <- "A1:H99999"
  intervalo2 <- "A1:H99999"
  
  sheet1 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2009-2010', range = intervalo1)
  sheet2 <- read_excel('online_retail_II.xlsx', sheet = 'Year 2010-2011', range = intervalo2)
  
  dados_combinados <- rbind(sheet1, sheet2)
  return(dados_combinados)
}

# Executa a função

dados <- carrega_dados()

dim(dados)
View(dados)


# Em Análise RFM não podemos ter dados NA


# função para chegar valores ausentes (recebe um objeto de dados (x) como entrada e retorna a soma de valores ausentes em cada coluna do objeto de dados. A função utiliza a função is.na() para verificar se cada valor em x é ausente (NA) e, em seguida, utiliza a função colSums() para somar o número de valores ausentes em cada coluna.)

funcao_verifica_valor_ausente <- function(x) {
  return(colSums(is.na(x)))
}

funcao_verifica_valor_ausente(dados)    # exibe as tabelas e a quantidade de valores NA em cada tabela


# Tomar a decisão sobre o que fazer com os valores ausentes.
#
# Analisando os valores, percebemos que a coluna Description tem 1169 valores ausentes. E neste caso específico como são
# muitos dados, então não irá fazer a diferença excluir todas as linhas cujo os valores de Description forem ausentes
#
# Analisando os valores, percebemos que a coluna Description tem 6252 valores ausentes. Como são valores de ID, não podemos fazer 
# imputação aqui (como fazer todos os valores NA ficarem com a média ou adicionar a moda a esses valores), então iremos excluir estas linhas


# Excluindo os registros/linhas com valores ausentes

# Função para limpar e pré-processar os dados (cria uma nova coluna, exclui valores ausentes, edita coluna Invoice)

funcao_processa_dados <- function(data) {
  
  # criando uma coluna chamada TotalPrice
  data$TotalPrice <- data$Quantity * data$Price
  
  # remove os registros/linhas com valores ausentes
  data <- na.omit(data)
  
  # removendo os registros/linhas da coluna Invoice que contém a letra C (o que significa que este pedido foi cancelado)
  data <- data[!grepl("C", data$Invoice), ]
  
  return(data)
}

# Executa a função

dataset <- funcao_processa_dados(dados)

dim(dataset)
View(dataset)


# Realziando a análise exploratória (pode se feita no início, após limpeza dos dados ou até mesmo dps de realizar engenharia de atributos)


# Verificando a distribuição da variável Total Price

ggplot(dataset,
       aes(x = TotalPrice)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 3.5) +
  labs(title = 'Distribuição da Variável TotalPrice')

# editando a escala do eixo x (TotalPrice) para melhor visualização

ggplot(dataset, aes(x = TotalPrice)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.35) +
  labs(title = 'Distribuição da Variável TotalPrice') +
  scale_x_continuous(limits = c(0, 100),  # Define o intervalo dos valores do eixo x
                     breaks = seq(0, 100, by = 10),  # Define os rótulos dos intervalos no eixo x
                     labels = function(x) paste0("$", x))  # Define os rótulos dos valores do eixo x com o símbolo de dólar



# Verificando número de clientes

# desta forma retorna todas as linhas de Customer ID e assim não saberemos ao certo se são todos os clientes

length(dataset$`Customer ID`)  # aspas por causa do espaço no nome da coluna

# Para isso usamos o unique

num_customers <- length(unique(dataset$`Customer ID`))

# Imprimir o número de clientes únicos

cat("Número de clientes únicos:", num_customers)


# Total monetário gasto por cliente (total que cada cliente gastou)

# - código usa a biblioteca dplyr no R para realizar operações de agregação no conjundo de dados dataset
# - group_by(Customer ID): É uma função do dplyr usada para agrupar os dados pelo valor da coluna Customer ID. Isso cria grupos
#   separados com base nos valores únicos da coluna Customer ID.
# - summarise(Soma = sum(TotalPrice)): É uma função do dplyr que é usada para realizar a agregação dos dados dentro dos grupos criados
#   pela função group_by(). Neste caso, está sendo calculada a soma dos valores da coluna TotalPrice para cada grupo, e o resultado é
#   armazenado em uma nova coluna chamada Soma no resultado final.

total_gasto <- dataset %>%
  group_by(`Customer ID`) %>% 
  summarise(Soma = sum(TotalPrice))

# Criação do gráfico de barras
ggplot(total_gasto, aes(x = `Customer ID`, y = Soma, fill = `Customer ID`)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Gasto por Cliente", x = "Cliente ID", y = "Total Gasto")



# Com base nos dados precisamos tomar a decisão para escolher o valor da Recência, que é quando o cliente fez
# a última compra (pode ser mês, dia, hora)


# Criando uma data customizada (Março de 2011) que será o nosso valor da recência com base na maior data

# Olhando a maior data (última data que temos de uma compra)
max(dataset$InvoiceDate)

# Com base em uma decisão vamos escolher como valor de recência 31 de março 2011 

date1 <- as.Date.character("31/03/2011", "%d/%m/%Y")


# Com isso foi considerado a data 31/03/2011 como valor da recência.
# E considerando a data , qual foi a compra mais recente de cada cliente? É isso que a recência irá responder


# Após tomada de decisão de não usar o controle de hora, minuto e segundo e assim temos que ajustar a coluna de data

# função para converter as datas do formato POISxt para o formato Date (edita removendo os horários da coluna)

funcao_converte_data <- function(obj) {
  
  options(digits.secs = 3)
  
  return(as.Date(as.POSIXct(obj$InvoiceDate, 'GMT')))
}

# Executa a função

dataset$InvoiceDate <- funcao_converte_data(dataset)
View(dataset)


# Agora temos a coluna InvoiceDate com a data no formato correto para usar como valor da Recência
# E assim temos os valores necessários para realizar a Análise RFM


# função para calcular Recência, Frquência e Valor Monetário

# - A função recebe um objeto de dados (obj) como entrada, em seguida, utiliza a função group_by() para agrupar os dados
#   por Customer ID, ou seja, calcular as métricas para cada cliente individualmente.
# - A função utiliza a função summarise() para resumir os dados do grupo em métricas específicas. As métricas calculadas são:
#
#  Recency: calculada como a diferença em dias entre a data date1 (que foi definida previamente como "31/03/2011") e a data mais
#           recente de compra (max(InvoiceDate)) para cada cliente, convertendo o resultado em formato numérico.
#  Frequency: calculada como o número de registros (n()) no grupo, que representa a contagem de compras feitas por cada cliente.
#             Ela conta o número de ocorrências de cada "Customer ID" no data frame.         
#  Monetary: calculada como a soma dos valores de TotalPrice para cada cliente, representando o valor total gasto por cada cliente.
#  Primeira_Compra: calculada como a data mínima de compra (min(InvoiceDate)) para cada cliente, representando a data da primeira
#                   compra realizada por cada cliente.
#
# - Essas métricas são calculadas para cada cliente no objeto de dados obj e são armazenadas em um novo objeto que é retornado pela
#   função. Essas métricas são comumente usadas na análise RFM (Recency, Frequency, Monetary), uma técnica de segmentação de clientes
#   amplamente utilizada em marketing e análise de dados para identificar padrões de comportamento do cliente e direcionar estratégias
#   de marketing segmentadas.

funcao_calcula_rfm <- function(obj) {
  
  z <- obj %>%
    group_by(`Customer ID`) %>%
    summarise(Recency = as.numeric(date1 - max(InvoiceDate)),
              Frequency = n(),
              Monetary = sum(TotalPrice),
              Primeira_Compra = min(InvoiceDate))
  
  
  # Tomando a decisão de remover transações com valores acima do 3º Quartil e abaixo do 1º Quartil
  # E com isso estaremos removendo valores outliers especialmente porque vamos aplicar Machine Learning
  
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  
  IQR <- IQR(z$Monetary)
  
  z <- subset(z, z$Monetary >= (Q1 - 1.5*IQR) & z$Monetary <= (Q3 + 1.5*IQR)) # filtrando
  
  return(z)
}

# Executa a função

valores_rfm <- funcao_calcula_rfm(dataset)

View(valores_rfm)



# Agora temos coeficientes que representam informações sobre o padrão de compra de cada cliente
# Com isso através deste conjunto de dados e vamos treinar o modelo de aprendizado de máquina para Segmentação de Clientes



# Set seed

# Parte do trabalho feito em machine learning envolve um processo aleatório.
# Usamos o set.seed para que este processo aleatório sempre comece da mesma forma.

set.seed(1029)


# Aplicando machine learning com clusterização com kmneans. 
# Clusterização é uma técnica de aprendizado não supervisionado.


# Função para a segmentação de clientes com base nos valores RFM


# - Cria uma lista vazia
# - Filtra colunas Recency, Frequency e Monetary do objeto obj e armazena em dados_rfm.
# - Cria o modelo de clusterização usando o algoritmo k-means através da função kmeans() do R. O modelo é criado com 5 clusters e
#   com um limite máximo de 50 iterações.
# - A função então plota o modelo através da função fviz_cluster() do pacote factoextra, que recebe como parâmetros o modelo criado
#   anteriormente, os dados dados_rfm e outros parâmetros de configuração do gráfico.
# - Por fim, a função adiciona a coluna Customer ID ao dataframe dados_rfm, armazena os clusters em uma nova coluna chamada clusters
#   e retorna o resultado na lista resultados na variável data.

funcao_segmenta_cliente <- function(obj) {
  
  # cria uma lista
  resultados <- list()
  
  # Filtrando colunas
  dados_rfm <- select(obj, c('Recency', 'Frequency', 'Monetary'))
  
  # Cria o modelo
  modelo_kmeans <- kmeans(dados_rfm, center = 5, iter.max = 50)
  
  # Plot do modelo
  resultados$plot <- fviz_cluster(modelo_kmeans,
                                  data = dados_rfm,
                                  geom = c('point'),
                                  ellipse.type = 'euclid')
  
  # Organiza os dados
  dados_rfm$`Customer ID` <- obj$`Customer ID`
  dados_rfm$Clusters <- modelo_kmeans$cluster
  resultados$data <- dados_rfm
  
  return(resultados)
}

# Observando função vemos que o objeto final é resultados ( return(resultados) )
# E dentro de resultados foi adicionado primeiro o plot ( resultados$plot ) e em segundo o ( resultados$data )
# Por isso para ver o grafico usaremos o [1] e a tabela usaremos o [2]


# Executa a função

grafico <- funcao_segmenta_cliente(valores_rfm)[1]
grafico

tabela_rfm <- funcao_segmenta_cliente(valores_rfm)[2]

tabela_rfm <- as.data.frame(tabela_rfm)

View(tabela_rfm)



# Com isso identificamos 5 (esse nº foi escolhido acima) grupos de clientes por similaridade e através de
# tabela_rfm podemos ver a qual grupo este cliente pertence (data.Clusters). E assim dentro de cada grupo temos clientes similares
# Com isso podemos pegar dentro de cada grupo e verificar o nível de recência






