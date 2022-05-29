###################################################################
# Metodos Matriciais e Analise de Clusters - Avaliacao Final - T8"
# "Rafael Rocha - A56660250"
# "Maio/2021"
###################################################################

###################################################################
#            Analise e agrupamento de clientes 
#                  com machine learning
###################################################################

########################################
######## Carregando os pacotes #########

library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(gdata)
library(ggplot2)
library(cowplot)
library(dendextend)
library(NbClust)
library(factoextra)
library(hrbrthemes)
library(cluster)
library(DataExplorer)
library(psych)
library(proxy)
library(ClusterR)

#############################################
########### Importando o dataset ############

baseclientes <- read_csv(file="/Users/rafaeldesouza/Desktop/MetodosMatriciaisAnaliseDeClusters/Pesquisa_Clientes.csv") 

baseclientes <- rename(baseclientes,c('MediaSalarial'='Media Salarial Mensal (R$)'))
baseclientes <- rename(baseclientes,c('PontuacaoGasto'='Pontuacao gasto (1-100)'))
baseclientes <- rename(baseclientes,c('Frequencia'='Frequencia'))
baseclientes <- rename(baseclientes,c('NivelSatisfacao'='Nivel Satisfacao (1-5)'))

############################################
########### Descricao dos dados ############
# Cliente ID - Id do cliente cadastrado no programa de fidelização de 2020
# Sexo - Sexo
# Idade - Idade
# Media Salarial - Média salarial declarada no cadastro do programa de fidelização em Reais (mil por mês)
# Pontuacao Gasto - De 1 a 100, onde cada R$100 gastos em qualquer um dos três restaurantes da rede durante o ano de 2020 (salão e delivery) o cliente acumulou 1 ponto
# Frequencia - Número total de visitas em qualquer um dos restaurantes + quantidade de pedidos de entrega (direta) em 2020
# Nivel Satisfacao - Nível de satisfação com o serviço prestado (salão e delivery) em 2020 de 1 a 5 

##########################################
########### Primeiros ajustes ############
# Separando os id's dos clientes por nao serem inicialmente importantes na analise 
# mas serao importantes para que nos possibilite identifica-los depois
baseclientes_id <- baseclientes %>% select(`Cliente ID`)
baseclientes_cleaned <- baseclientes %>% select(-`Cliente ID`)
baseclientes_cleaned2 <- baseclientes %>% select(-`Sexo`, -`Cliente ID`)

# Vamos verificar quais das colunas possui 
# variaveis categoricas e transforma-las em numeros. 
# Lembrando que na utilizacao do R, as variaveis do tipo factor sao as variaveis 
# categoricas.
# Atributo names = nome das colunas
baseclientes %>% Filter(f = is.factor) %>% names # Base
#data %>% select_if(is.factor) %>% colnames() #Tidyverse

#############################################
########### Analise exploratoria ############

introduce(baseclientes)
plot_intro(baseclientes)
#create_report(baseclientes)

head(baseclientes)
summary(baseclientes)

# Visualizacao e descricoes estatisticas
plot_correlation(baseclientes_cleaned)

pairs.panels(baseclientes_cleaned, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#######################################
##### Boxplots divididos por sexo #####
b1 <- baseclientes %>% ggplot() + geom_boxplot(mapping = aes(x = Idade, y = Sexo, color = Sexo)) + 
  ggtitle("Boxplot - Idade")+ xlab("Idade") + coord_flip() + theme_classic()
b1

b2 <- baseclientes %>% ggplot() + geom_boxplot(mapping = aes(x = MediaSalarial, y = Sexo, colour = Sexo)) + 
  ggtitle("Boxplot - Media Salarial Mensal em mil reais")+ xlab("MediaSalarial") + coord_flip() + theme_classic()
b2

b3 <- baseclientes %>% ggplot() + geom_boxplot(mapping = aes(x = PontuacaoGasto, y = Sexo, colour = Sexo)) + 
  ggtitle("Boxplot - Pontuação Gasto de 1 a 100")+ xlab("PontuacaoGasto") + coord_flip() + theme_classic()
b3

b4 <- baseclientes %>% ggplot() + geom_boxplot(mapping = aes(x = Frequencia, y = Sexo, colour = Sexo)) + 
  ggtitle("Boxplot - Frequência")+ xlab("Frequencia") + coord_flip() + theme_classic()
b4

b5 <- baseclientes %>% ggplot() + geom_boxplot(mapping = aes(x = NivelSatisfacao, y = Sexo, colour = Sexo)) + 
  ggtitle("Boxplot - Nível Satisfação de 1 a 5")+ xlab("NivelSatisfacao") + coord_flip() + theme_classic()
b5

############################################
##### Analise de distribuicao por sexo #####

d1 <- baseclientes %>% ggplot() + geom_histogram(mapping = aes(x = Idade, fill = Sexo),bins = 10, position = "dodge", colour = "black") + 
  ggtitle("Distribuição por Idade ")+ xlab("Idade") + ylab("Total Count") + theme_classic()
d1

d2 <- baseclientes %>% ggplot() + geom_histogram(mapping = aes(x = MediaSalarial, fill = Sexo),bins = 10, position = "dodge", colour = "black") + 
  ggtitle("Distribuição por Media Salarial Mensal em mil reais")+ xlab("MediaSalarial") + ylab("Total Count") + theme_classic()
d2

d3 <- baseclientes %>% ggplot() + geom_histogram(mapping = aes(x = PontuacaoGasto, fill = Sexo),bins = 10, position = "dodge", colour = "black") + 
  ggtitle("Distribuição por Pontuação Gasto de 1 a 100")+ xlab("PontuacaoGasto") + ylab("Total Count") + theme_classic()
d3

d4 <- baseclientes %>% ggplot() + geom_histogram(mapping = aes(x = Frequencia, fill = Sexo),bins = 10, position = "dodge", colour = "black") + 
  ggtitle("Distribuição por Frequência")+ xlab("Frequencia") + ylab("Total Count") + theme_classic()
d4

d5 <- baseclientes %>% ggplot() + geom_histogram(mapping = aes(x = NivelSatisfacao, fill = Sexo),bins = 10, position = "dodge", colour = "black") + 
  ggtitle("Distribuição Nível Satisfação de 1 a 5")+ xlab("NivelSatisfacao") + ylab("Total Count") + theme_classic()
d5

######################################
########### CLUSTERIZACAO ############
######################################

######## Metodo do Cotovelo #########

# Tentativa com Idade, Media Salarial, Pontuacao Gasto, Frequencia e Nivel de Satisfacao
opt <- Optimal_Clusters_KMeans(baseclientes[, 3:7], max_clusters = 10, plot_clusters = T)

# Tentativa apenas com Idade, Media Salarial e Pontuacao Gasto
opt <- Optimal_Clusters_KMeans(baseclientes[, 3:5], max_clusters = 10, plot_clusters = T)

# Tentativa apenas com Media Salarial, Pontuacao Gasto e Frequencia
opt <- Optimal_Clusters_KMeans(baseclientes[, 4:6], max_clusters = 10, plot_clusters = T)
# Utilizaremos esse ultimo - Metodo do cotovelo sugere o uso de 3 clusters

########### Silhueta ############

# Tentativa com Idade, Media Salarial, Pontuacao Gasto, Frequencia e Nivel de Satisfacao
opt <- Optimal_Clusters_KMeans(baseclientes[, 3:7], max_clusters = 10, plot_clusters = T, criterion = 'silhouette')

# Tentativa apenas com Idade, Media Salarial e Pontuacao Gasto
opt <- Optimal_Clusters_KMeans(baseclientes[, 3:5], max_clusters = 10, plot_clusters = T, criterion = 'silhouette')

# Tentativa apenas com Media Salarial e Pontuacao Gasto e Frequencia
opt <- Optimal_Clusters_KMeans(baseclientes[, 4:6], max_clusters = 10, plot_clusters = T, criterion = 'silhouette')
# Utilizaremos esse ultimo - Silhueta tambem sugere 3 clusters (maior numero encontrado)

########### Clusterizacao com k-means ############

# Usando Media Salarial, Pontuacao Gasto e Frequencia, baseado nos resultados do cotovelo e silhueta (3)
set.seed(22)
km <- kmeans(baseclientes[,4:6], 3, nstart=25)
print(km)
baseclientes$ClusterNumber <- km$cluster

ggplot(baseclientes[,1:7])  +
  geom_point(aes(x = MediaSalarial, y = PontuacaoGasto, col = as.factor(baseclientes$ClusterNumber))) +
  scale_color_discrete(name="Cluster Number")

# As médias das variáveis para os agrupamentos podem ser obtidas por meio da função abaixo
cluster_result <- aggregate(baseclientes[,4:6], by=list(cluster=km$cluster), mean)
cluster_result

# cluster plot 2
df2 <- cbind(baseclientes[,1:7], cluster=km$cluster)

a <- fviz_cluster(km, data=df2,
                  palette = c("#2E9FDF", "#E46726", "#E7B800"),
                  #ellipse.type="euclid",
                  star.plot=TRUE,
                  repel=TRUE,
                  main = "Visualização do resultado dos clusters")

a + theme_ipsum()

########### Plotando com clusplot ############

baseclientes.x <- baseclientes[, 4:6]
cl3 <- pam(baseclientes.x, 3)$clustering
op <- par(mfrow= c(2,2))
clusplot(baseclientes.x, cl3, color = TRUE,
         main = paste('Clusterizacao dos Clientes'),
         xlab = 'MediaSalarial',
         ylab = 'PontuacaoGasto')


##################################################
########### Clusterizacao Hierarquica ############

dist_baseclientes <- dist(baseclientes[,4:6])
hc_baseclientes <- hclust(dist_baseclientes, method = 'complete')

########### Silhueta ############
for(i in 2:7) { 
  nam <- paste("clust", i, sep = "")
  assign(nam, cutree(hc_baseclientes, k = i))
}

par(mfrow = c(3, 2))

plot(silhouette(clust2, dist_baseclientes), col = "blue")
plot(silhouette(clust3, dist_baseclientes), col = 'blue')
plot(silhouette(clust4, dist_baseclientes), col = "blue")
plot(silhouette(clust5, dist_baseclientes), col = "blue")

# resultado sugere uso de 3 clusters com resultado 0.54 (sendo o maior dos resultados)

# plotando o dendograma
clust_baseclientes <- cutree(hc_baseclientes, k = 3)
dendograma_baseclientes <- as.dendrogram(hc_baseclientes)
dendograma_cores <- color_branches(dendograma_baseclientes, k = 3)
par(mfrow = c(1, 1))
plot(dendograma_cores)

##########################################
########### Clusterizacao PAM ############

baseclientes.scaled <- scale(baseclientes[,2:7])
pam.res <- pam(baseclientes.scaled, 3)

###### Visualisando o pam clustering #####
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm", main = paste('Clusterização dos clientes com critério PAM'),)

##########################################
###### Relacionando as variáveis #########
##########################################

segment_customers <- mutate(baseclientes, cluster = clust_baseclientes)

segment_customers = subset(segment_customers, select = -c(ClusterNumber))

ggplot(segment_customers, aes(x = MediaSalarial, y = PontuacaoGasto, color = factor(cluster))) +
  geom_point() +
  scale_color_discrete(name = 'Cluster number')

segment_customers %>% group_by(cluster, Sexo) %>%
  summarise_all(list(mean)) %>% arrange(cluster)


