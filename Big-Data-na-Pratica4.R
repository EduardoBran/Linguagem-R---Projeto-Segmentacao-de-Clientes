# Trabalhando com R e SQLite


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


# Instalando e importando pacotes

library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)
