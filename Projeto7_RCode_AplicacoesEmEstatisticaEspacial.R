################################################################
# "Aplicações em Estatística Espacial - Avaliação Final - T8"
# "Rafael Rocha - A56660250"
# "Março/2021"
################################################################

################################################################
# Mapeamento e análise espacial do número de casos e óbitos 
#          de COVID-19 no estado do Rio de Janeiro
#
#           Período de março a dezembro de 2020
################################################################

# Carregando os pacotes
library(dplyr)
library(spatstat)
library(spatstat.data)
library(sf)
library(tidyverse)
library(ggmap)
library(tmap)
library(ggplot2)
library(spatstat)
library(rgdal)
library(rmapshaper)
library(sp)
library(maptools)
library(gstat)
library(doBy)
library(ggmap)
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(MASS)
library(spdep)
library(cartography)
library(spatialreg)
library(corrplot)
library(stringr)

options(scipen=7)

#############################################
############## Importando a base ############

base_covid_rj <- read_delim("base_obitos_casos_mes_rj.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "ISO-8859-1"), trim_ws = TRUE)

# Verificando resumo dos dados
summary(base_covid_rj)

# Verificando estrutura dos dados
str(base_covid_rj)

# Visualizando as primeiras linhas da base
head(base_covid_rj)

################################################
############ Importando o shapefile ############

RJshp = read_sf("municipiosrj.shp")

# Simplificando o shape
RJshp <- ms_simplify(input = RJshp,
                     keep = 0.02,
                     keep_shapes = TRUE)

# Plotando o mapa
ggplot(RJshp) +
  geom_sf(fill = "lightblue")

## Inserindo os dados no shape
RJshp = merge(RJshp, base_covid_rj, by.x = "GEOCODIGO", by. ="codibge")

###########################################################
############ Agregando os dados para a analise ############
###########################################################

####################### POR CASOS #########################

## Por TOTAL de casos por municipio (MARÇO A DEZEMBRO)
RJshp = RJshp %>% 
  summarise(RJshp, total_casos = c(mar_casos+abril_casos+maio_casos+junho_casos+julho_casos+agosto_casos+setembro_casos+outubro_casos+novembro_casos+dezembro_casos)) 

base_covid_rj = base_covid_rj %>% 
  summarise(base_covid_rj, total_casos = c(mar_casos+abril_casos+maio_casos+junho_casos+julho_casos+agosto_casos+setembro_casos+outubro_casos+novembro_casos+dezembro_casos))
            
## Por total de casos de MARÇO A JULHO por municipio
RJshp = RJshp %>%  
  summarise(RJshp, total_casos_mar_julho = c(mar_casos+abril_casos+maio_casos+junho_casos+julho_casos)) 

base_covid_rj = base_covid_rj %>%  
  summarise(base_covid_rj, total_casos_mar_julho = c(mar_casos+abril_casos+maio_casos+junho_casos+julho_casos))

## Por total de casos AGOSTO A DEZEMBRO por municipio
RJshp = RJshp %>%
  summarise(RJshp, total_casos_agosto_dezembro = c(agosto_casos+setembro_casos+outubro_casos+novembro_casos+dezembro_casos)) 

base_covid_rj = base_covid_rj %>%           
  summarise(base_covid_rj, total_casos_agosto_dezembro = c(agosto_casos+setembro_casos+outubro_casos+novembro_casos+dezembro_casos))

###################### POR OBITOS #########################

## Por total de obitos por municipio (MARÇO A DEZEMBRO)
RJshp = RJshp %>% 
  summarise(RJshp, total_obitos = c(mar_obitos+abril_obitos+maio_obitos+junho_obitos+julho_obitos+agosto_obitos+setembro_obitos+outubro_obitos+novembro_obitos+dezembro_obitos))                              

base_covid_rj = base_covid_rj %>% 
  summarise(base_covid_rj, total_obitos = c(mar_obitos+abril_obitos+maio_obitos+junho_obitos+julho_obitos+agosto_obitos+setembro_obitos+outubro_obitos+novembro_obitos+dezembro_obitos))

## Por total de casos de MARÇO A JULHO por municipio
RJshp = RJshp %>%  
  summarise(RJshp, total_obitos_mar_julho = c(mar_obitos+abril_obitos+maio_obitos+junho_obitos+julho_obitos)) 

base_covid_rj = base_covid_rj %>% 
  summarise(base_covid_rj, total_obitos_mar_julho = c(mar_obitos+abril_obitos+maio_obitos+junho_obitos+julho_obitos))

## Por total de casos AGOSTO A DEZEMBRO por municipio
RJshp = RJshp %>%
  summarise(RJshp, total_obitos_agosto_dezembro = c(agosto_obitos+setembro_obitos+outubro_obitos+novembro_obitos+dezembro_obitos))

base_covid_rj = base_covid_rj %>% 
  summarise(base_covid_rj, total_obitos_agosto_dezembro = c(agosto_obitos+setembro_obitos+outubro_obitos+novembro_obitos+dezembro_obitos))

#########################################################################
############ Transformando as variaveis de proporcao na base ############
#########################################################################

RJshp = RJshp %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100) # Essas transformacoes vao ajudar na modelagem 

##############################################################
################### Avaliacao visual #########################
##############################################################

######################## CASOS ###############################

###### TOTAL de CASOS por municipio (MARÇO A DEZEMBRO) #######

# Definindo o tipo do mapa como estatico
tmap_mode("plot")

# Construindo o mapa para visualizar total de casos por municipio 
tm_shape(RJshp) + 
  tm_fill(col = "total_casos",
          breaks = quantile(RJshp$total_casos, probs = seq(0,1,0.080)),
          palette = "BuPu",
          title = "TOTAL DE CASOS POR MUNICÍPIO - MARÇO A DEZEMBRO DE 2020") +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

RJshp = RJshp %>% 
  mutate(total_casos2 = str_c(mun, " - ", total_casos))

#Definindo o tipo do mapa como estatico
tmap_mode("view")

#Construindo o mapa para visualizar total de casos por municipio
tm_shape(RJshp) + 
  tm_fill(col = "total_casos",
          breaks = quantile(RJshp$total_casos, probs = seq(0,1,0.080)),
          palette = "Oranges",
          title = "TOTAL DE CASOS POR MUNICÍPIO - MARÇO A DEZEMBRO DE 2020",
          id = "total_casos2",
          alpha = 1) +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

####### TOTAL de CASOS por municipio (MARÇO A JULHO) ########

# Definindo o tipo do mapa como estatico
tmap_mode("plot")

# Construindo o mapa para visualizar total de casos por municipio 
tm_shape(RJshp) + 
  tm_fill(col = "total_casos_mar_julho",
          breaks = quantile(RJshp$total_casos_mar_julho, probs = seq(0,1,0.080)),
          palette = "BuPu",
          title = "CASOS POR MUNICÍPIO - MARÇO A JULHO DE 2020") +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

RJshp = RJshp %>% 
  mutate(total_casos3 = str_c(mun, " - ", total_casos_mar_julho))

#Definindo o tipo do mapa como estatico
tmap_mode("view")

#Construindo o mapa para visualizar total de casos por municipio
tm_shape(RJshp) + 
  tm_fill(col = "total_casos_mar_julho",
          breaks = quantile(RJshp$total_casos_mar_julho, probs = seq(0,1,0.080)),
          palette = "Oranges",
          title = "CASOS POR MUNICÍPIO - MARÇO A JULHO DE 2020",
          id = "total_casos3",
          alpha = 1) +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

####### TOTAL de CASOS por municipio (AGOSTO A DEZEMBRO) ########

# Definindo o tipo do mapa como estatico
tmap_mode("plot")

# Construindo o mapa para visualizar total de casos por municipio 
tm_shape(RJshp) + 
  tm_fill(col = "total_casos_agosto_dezembro",
          breaks = quantile(RJshp$total_casos_agosto_dezembro, probs = seq(0,1,0.080)),
          palette = "BuPu",
          title = "CASOS POR MUNICÍPIO - AGOSTO A DEZEMBRO DE 2020") +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

RJshp = RJshp %>% 
  mutate(total_casos4 = str_c(mun, " - ", total_casos_agosto_dezembro))

#Definindo o tipo do mapa como estatico
tmap_mode("view")

#Construindo o mapa para visualizar total de casos por municipio
tm_shape(RJshp) + 
  tm_fill(col = "total_casos_agosto_dezembro",
          breaks = quantile(RJshp$total_casos_agosto_dezembro, probs = seq(0,1,0.080)),
          palette = "Oranges",
          title = "CASOS POR MUNICÍPIO - AGOSTO A DEZEMBRO DE 2020",
          id = "total_casos4",
          alpha = 1) +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

##############################################################
################### Avaliacao visual #########################
##############################################################

######################## OBITOS ##############################

###### TOTAL de OBITOS por municipio (MARÇO A DEZEMBRO) #######

# Definindo o tipo do mapa como estatico
tmap_mode("plot")

# Construindo o mapa para visualizar total de obitos por municipio 
tm_shape(RJshp) + 
  tm_fill(col = "total_obitos",
          breaks = quantile(RJshp$total_obitos, probs = seq(0,1,0.080)),
          palette = "Greens",
          title = "TOTAL DE ÓBITOS POR MUNICÍPIO - MARÇO A DEZEMBRO DE 2020") +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

RJshp = RJshp %>% 
  mutate(total_obitos2 = str_c(mun, " - ", total_obitos))

#Definindo o tipo do mapa como estatico
tmap_mode("view")

#Construindo o mapa para visualizar total de obitos por municipio
tm_shape(RJshp) + 
  tm_fill(col = "total_obitos",
          breaks = quantile(RJshp$total_obitos, probs = seq(0,1,0.080)),
          palette = "YlGnBu",
          title = "TOTAL DE ÓBITOS POR MUNICÍPIO - MARÇO A DEZEMBRO DE 2020",
          id = "total_obitos2",
          alpha = 1) +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

####### TOTAL de ÓBITOS por municipio (MARÇO A JULHO) ########

# Definindo o tipo do mapa como estatico
tmap_mode("plot")

# Construindo o mapa para visualizar total de obitos por municipio 
tm_shape(RJshp) + 
  tm_fill(col = "total_obitos_mar_julho",
          breaks = quantile(RJshp$total_obitos_mar_julho, probs = seq(0,1,0.080)),
          palette = "Greens",
          title = "ÓBITOS POR MUNICÍPIO - MARÇO A JULHO DE 2020") +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

RJshp = RJshp %>% 
  mutate(total_obitos3 = str_c(mun, " - ", total_obitos_mar_julho))

#Definindo o tipo do mapa como estatico
tmap_mode("view")

#Construindo o mapa para visualizar total de obitos por municipio
tm_shape(RJshp) + 
  tm_fill(col = "total_obitos_mar_julho",
          breaks = quantile(RJshp$total_obitos_mar_julho, probs = seq(0,1,0.080)),
          palette = "YlGnBu",
          title = "ÓBITOS POR MUNICÍPIO - MARÇO A JULHO DE 2020",
          id = "total_obitos3",
          alpha = 1) +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

####### TOTAL de OBITOS por municipio (AGOSTO A DEZEMBRO) ########

# Definindo o tipo do mapa como estatico
tmap_mode("plot")

# Construindo o mapa para visualizar total de obitos por municipio 
tm_shape(RJshp) + 
  tm_fill(col = "total_obitos_agosto_dezembro",
          breaks = quantile(RJshp$total_obitos_agosto_dezembro, probs = seq(0,1,0.080)),
          palette = "Greens",
          title = "ÓBITOS POR MUNICÍPIO - AGOSTO A DEZEMBRO DE 2020") +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))

RJshp = RJshp %>% 
  mutate(total_obitos4 = str_c(mun, " - ", total_obitos_agosto_dezembro))

#Definindo o tipo do mapa como estatico
tmap_mode("view")

#Construindo o mapa para visualizar total de obitos por municipio
tm_shape(RJshp) + 
  tm_fill(col = "total_obitos_agosto_dezembro",
          breaks = quantile(RJshp$total_obitos_agosto_dezembro, probs = seq(0,1,0.080)),
          palette = "YlGnBu",
          title = "ÓBITOS POR MUNICÍPIO - AGOSTO A DEZEMBRO DE 2020",
          id = "total_obitos4",
          alpha = 1) +
  tm_borders() +
  tm_view(view.legend.position = c("right","bottom"))


#########################################################
############# Avaliacao de autocorrelacao ###############
#########################################################

#################### TOTAL CASOS ########################

#Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
dados_casos <- base_covid_rj %>% 
  dplyr::select(total_casos, ivs, idhm2010, pop_nbranca, prop_ab, prop_cad, prop_id)

dados_casos = dados_casos %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100)

correlacao = cor(dados_casos)
corrplot(correlacao)

##############################################################
#Avaliando autocorrelacao AVANÇADA entre as variaveis explicativas e a variavel resposta

library(PerformanceAnalytics)

CorrelacaoAvancadaCasos <- dados_casos[,c(1, 2, 3, 4, 5, 6, 7)] #filtrando apenas colunas relevantes

chart.Correlation(CorrelacaoAvancadaCasos, histogram = TRUE)

#Definindo o tipo do mapa como estatico
tmap_mode("plot")

#Construindo o mapa para as variaveis (resposta e explicativas) 
tm_shape(RJshp) + 
  tm_fill(col = c("total_casos","ivs","idhm2010","pop_nbranca"),
          breaks = list(quantile(RJshp$total_casos, probs = seq(0,1,0.2)),
                        quantile(RJshp$ivs, probs = seq(0,1,0.2)),
                        quantile(RJshp$idhm2010, probs = seq(0,1,0.2)),
                        quantile(RJshp$pop_nbranca, probs = seq(0,1,0.2))),
          palette = "GnBu",
          title = c("TOTAL DE CASOS",
                    "ÍNDICE DE VUNERABILIDADE SOCIAL",
                    "ÍNDICE DE DESENVOLVIMENTO HUMANO PARA 2010",
                    "POPULAÇÃO NÃO BRANCA"))

tm_shape(RJshp) + 
  tm_fill(col = c("prop_ab", "prop_cad", "prop_id"),
          breaks = list(quantile(RJshp$prop_ab, probs = seq(0,1,0.2)),
                        quantile(RJshp$prop_cad, probs = seq(0,1,0.2)),
                        quantile(RJshp$prop_id, probs = seq(0,1,0.2))),
          palette = "GnBu",
          title = c("POPULAÇÃO COM ACESSO A REDE DE ATENÇÃO BÁSICA",
                    "POPULAÇÃO REGISTRADA NO CADASTRO ÚNICO", 
                    "POPULAÇÃO COM IDADE ACIMA DE 60 ANOS"))

################# TOTAL CASOS - MARCO A JULHO #####################

#Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
dados_casos_mar_julho <- base_covid_rj %>% 
  dplyr::select(total_casos_mar_julho, ivs, idhm2010, pop_nbranca, prop_ab, prop_cad, prop_id)

dados_casos_marco_julho = dados_casos_mar_julho %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100)

correlacao = cor(dados_casos_mar_julho)
corrplot(correlacao)

################# TOTAL CASOS - AGOSTO A DEZEMBRO #####################

#Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
dados_casos_agosto_dezembro <- base_covid_rj %>% 
  dplyr::select(total_casos_agosto_dezembro, ivs, idhm2010, pop_nbranca, prop_ab, prop_cad, prop_id)

dados_casos_agosto_dezembro = dados_casos_agosto_dezembro %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100)

correlacao = cor(dados_casos_agosto_dezembro)
corrplot(correlacao)

#########################################################
############# Avaliacao de autocorrelacao ###############
#########################################################

################### TOTAL OBITOS ########################

#Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
dados_obitos <- base_covid_rj %>% 
  dplyr::select(total_obitos, leitos_sus, resps_total, ivs, idhm2010, pop_nbranca, prop_ab, prop_cad, prop_id)

dados_obitos = dados_obitos %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100)

correlacao = cor(dados_obitos)
corrplot(correlacao)

##############################################################
#Avaliando autocorrelacao AVANÇADA entre as variaveis explicativas e a variavel resposta

CorrelacaoAvancadaCasos <- dados_obitos[,c(1, 2, 3, 4, 5, 6, 7, 8 ,9)] #filtrando apenas colunas relevantes

chart.Correlation(CorrelacaoAvancadaCasos, histogram = TRUE)

#Definindo o tipo do mapa como estatico
tmap_mode("plot")

#Construindo o mapa para as variaveis (resposta e explicativas) 
tm_shape(RJshp) + 
  tm_fill(col = c("total_obitos","leitos_sus","resps_total"),
          breaks = list(quantile(RJshp$total_obitos, probs = seq(0,1,0.2)),
                        quantile(RJshp$leitos_sus, probs = seq(0,1,0.2)),
                        quantile(RJshp$resps_total, probs = seq(0,1,0.2))),
          palette = "BrBG",
          title = c("TOTAL DE ÓBITOS",
                    "LEITOS SUS POR 100MIL HABITANTES",
                    "TOTAL DE RESPIRADORES"))

tm_shape(RJshp) + 
  tm_fill(col = c("ivs", "idhm2010", "pop_nbranca"),
          breaks = list(quantile(RJshp$ivs, probs = seq(0,1,0.2)),
                        quantile(RJshp$idhm2010, probs = seq(0,1,0.2)),
                        quantile(RJshp$pop_nbranca, probs = seq(0,1,0.2))),
          palette = "BrBG",
          title = c("ÍNDICE DE VUNERABILIDADE SOCIAL",
                    "ÍNDICE DE DESENVOLVIMENTO HUMANO PARA 2010",
                    "POPULAÇÃO NÃO BRANCA"))

tm_shape(RJshp) + 
  tm_fill(col = c("prop_ab", "prop_cad", "prop_id"),
          breaks = list(quantile(RJshp$prop_ab, probs = seq(0,1,0.2)),
                        quantile(RJshp$prop_cad, probs = seq(0,1,0.2)),
                        quantile(RJshp$prop_id, probs = seq(0,1,0.2))),
          palette = "BrBG",
          title = c("POPULAÇÃO COM ACESSO A REDE DE ATENÇÃO BÁSICA",
                    "POPULAÇÃO REGISTRADA NO CADASTRO ÚNICO", 
                    "POPULAÇÃO COM IDADE ACIMA DE 60 ANOS"))

################# TOTAL OBITOS - MARCO A JULHO #####################

#Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
dados_obitos_mar_julho <- base_covid_rj %>% 
  dplyr::select(total_obitos_mar_julho, leitos_sus, resps_total, ivs, idhm2010, pop_nbranca, prop_ab, prop_cad, prop_id)

dados_obitos_mar_julho = dados_obitos_mar_julho %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100)

correlacao = cor(dados_obitos_mar_julho)
corrplot(correlacao)

################# TOTAL OBITOS - AGOSTO A DEZEMBRO #####################

#Avaliando autocorrelacao entre as variaveis explicativas e a variavel resposta
dados_obitos_agosto_dezembro <- base_covid_rj %>% 
  dplyr::select(total_obitos_agosto_dezembro, leitos_sus, resps_total, ivs, idhm2010, pop_nbranca, prop_ab, prop_cad, prop_id)

dados_obitos_agosto_dezembro = dados_obitos_agosto_dezembro %>% 
  mutate(ivs = ivs*100,
         idhm2010 = idhm2010*100,
         prop_ab = prop_ab*100,
         prop_cad = prop_cad*100,
         prop_id = prop_id*100)

correlacao = cor(dados_obitos_agosto_dezembro)
corrplot(correlacao)

##############################################################
#################### Mapas de vizinhança #####################
##############################################################

## W com o criterio queen
W.queen = poly2nb(pl = RJshp, 
                  queen = TRUE)

## pesos padronizado por linhas
W.Queen.pesoW <- nb2listw(neighbours = W.queen, 
                          style="W") #outras opcoes: B, C, S e U

## pesos binario
W.Queen.pesoB = nb2listw(neighbours = W.queen,
                         style="B") #outras opcoes: B, C, S e U


## Criando a matriz W com 3 vizinhos para cada regiao e com peso padronizado por linha #

#Extraindo os centroides do shape
centroides = st_centroid(RJshp)

#Extraindo os k vizinhos mais proximos
k3viz = knearneigh(x = centroides, 
                   k = 3)

## knearneigh - define os k vizinhos mais proximos
#Argumentos:
#x - objeto do tipo SpatialPoints
#k - numero de vizinhos

#Contruindo W com 3 vizinhos
W.3viz = knn2nb(knn = k3viz)



##############################################################
################### Autocorrelacao global ####################

    ################# Total de casos  ####################

#Moran global com W baseada no criterio QUEEN
# e peso padronizado por linha
moran.test(x = RJshp$total_casos, 
           listw = W.Queen.pesoW)


#Moran global com W baseada no criterio QUEEN
# e peso binario
moran.test(x = RJshp$total_casos, 
           listw = W.Queen.pesoB)

#Calculando o Moran Local
moranlocREC = localmoran(x = RJshp$total_casos,
                         listw = W.Queen.pesoW, 
                         na.action = na.exclude, 
                         zero.policy = TRUE)

#Acrescentando o Moran local no shape
RJshp$Moran_local = moranlocREC[,1]

#Definindo o tipo do mapa como interativo
tmap_mode("plot")

tm_shape(RJshp) + 
  tm_fill(col = "Moran_local",
          n = 5,
          palette = "Oranges",
          title = "Moran Local") +
  tm_borders()

####### Temos indicacoes de que existe autocorrelacao espacial global nesse contexto
####### portando essas areas citadas nao podem ser consideradas como independentes

#Ajustando um modelo SAR1
ajusteSAR1 = spautolm(formula = log(total_casos) ~ ivs + sqrt(pop_nbranca) + prop_cad,  
                      data = base_covid_rj, 
                      listw = W.Queen.pesoW, 
                      family = "SAR")

summary(ajusteSAR1)

#Ajustando um modelo SAR2
ajusteSAR2 = spautolm(formula = log(total_casos) ~ ivs + sqrt(pop_nbranca) + prop_cad, 
                      data = base_covid_rj, 
                      listw = W.Queen.pesoB, 
                      family = "SAR")

summary(ajusteSAR2)

#Ajustando um modelo CAR
ajusteCAR = spautolm(formula = log(total_casos) ~ ivs + sqrt(pop_nbranca) + prop_cad,
                     data = base_covid_rj, 
                     listw = W.Queen.pesoB, 
                     family = "CAR")

summary(ajusteCAR)

#Ajustando um modelo CAR2
ajusteCAR2 = spautolm(formula = log(total_casos) ~ sqrt(ivs) + sqrt(pop_nbranca) + sqrt(prop_cad),
                     data = base_covid_rj, 
                     listw = W.Queen.pesoB, 
                     family = "CAR")

summary(ajusteCAR2)

#Comparando o AIC
AIC(ajusteSAR1)
AIC(ajusteSAR2)
AIC(ajusteCAR)
AIC(ajusteCAR2)

AIC(ajusteSAR1, ajusteSAR2, ajusteCAR, ajusteCAR2)

      ################# Total de obitos ####################

#Moran global com W baseada no criterio QUEEN
# e peso padronizado por linha
moran.test(x = RJshp$total_obitos, 
           listw = W.Queen.pesoW)


#Moran global com W baseada no criterio QUEEN
# e peso binario
moran.test(x = RJshp$total_obitos, 
           listw = W.Queen.pesoB)

#Calculando o Moran Local
moranlocREC = localmoran(x = RJshp$total_obitos,
                         listw = W.Queen.pesoW, 
                         na.action = na.exclude, 
                         zero.policy = TRUE)

#Acrescentando o Moran local no shape
RJshp$Moran_local = moranlocREC[,1]

#Definindo o tipo do mapa como interativo
tmap_mode("plot")

tm_shape(RJshp) + 
  tm_fill(col = "Moran_local",
          n = 5,
          palette = "Oranges",
          title = "Moran Local") +
  tm_borders()

#Ajustando um modelo SAR1 
ajusteSAR1 = spautolm(formula = log(total_obitos) ~ sqrt(leitos_sus) + sqrt(resps_total) + sqrt(pop_nbranca) + sqrt(prop_cad),  
                      data = base_covid_rj, 
                      listw = W.Queen.pesoW, 
                      family = "SAR")

summary(ajusteSAR1)

#Ajustando um modelo SAR2
ajusteSAR2 = spautolm(formula = log(total_obitos) ~ sqrt(leitos_sus) + sqrt(resps_total) + sqrt(pop_nbranca) + sqrt(prop_cad), 
                      data = base_covid_rj, 
                      listw = W.Queen.pesoB, 
                      family = "SAR")

summary(ajusteSAR2)

#Ajustando um modelo CAR
ajusteCAR = spautolm(formula = log(total_obitos) ~ sqrt(leitos_sus) + sqrt(resps_total) + sqrt(pop_nbranca) + sqrt(prop_cad),
                     data = base_covid_rj, 
                     listw = W.Queen.pesoB, 
                     family = "CAR")

summary(ajusteCAR)

#Ajustando um modelo CAR2
ajusteCAR2 = spautolm(formula = log(total_obitos) ~ sqrt(leitos_sus) + sqrt(resps_total) + sqrt(pop_nbranca) + sqrt(prop_cad),
                     data = base_covid_rj, 
                     listw = W.Queen.pesoB, 
                     family = "CAR")

summary(ajusteCAR2)

#Comparando o AIC
AIC(ajusteSAR1)
AIC(ajusteSAR2)
AIC(ajusteCAR)
AIC(ajusteCAR2)

AIC(ajusteSAR1, ajusteSAR2, ajusteCAR, ajusteCAR2)
