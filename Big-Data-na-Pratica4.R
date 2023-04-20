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

verifica_valor_ausente <- function(x) {
  return(colSums(is.na(x)))
}

verifica_valor_ausente(dados)    # exibe as tabelas e a quantidade de valores NA em cada tabela


# Tomar a decisão sobre o que fazer com os valores ausentes.
#
# Analisando os valores, percebemos que a coluna Description tem 1169 valores ausentes. E neste caso específico como são
# muitos dados, então não irá fazer a diferença excluir todas as linhas cujo os valores de Description forem ausentes
#
# Analisando os valores, percebemos que a coluna Description tem 6252 valores ausentes. Como são valores de ID, não podemos fazer 
# imputação aqui (como fazer todos os valores NA ficarem com a média ou adicionar a moda a esses valores), então iremos excluir estas linhas


# Excluindo os registros/linhas com valores ausentes

# Função para limpar e pré-processar os dados
