################################
# "Estatística Espacial - Avaliação Final - T8"
# "Rafael Rocha - A56660250"
# "Fevereiro/2021"
################################

##########################################################
# Mapeamento e análise de crimes ocorridos em Houston, USA
# Período de janeiro a agosto de 2010
##########################################################

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

# Importando a base "Base Houston.csv"
base <- read.csv2(file.choose(), head=T, sep = ',', dec = ".")

# Verificando resumo dos dados
summary(base)

# Verificando estrutura dos dados
str(base)

# Visualizando as primeiras linhas da base
head(base)

# O foco está nas colunas: **offense**, **lon** e **lat** que são as de nosso interesse. Se nota que as 
# coordenadas geográficas estão em lat long, que em resumo formam um padrão de grade sobre a Terra, 
# possibilizando apontar a localização exata de qualquer local no globo. 
# Posteriormente, verificaremos se # o shapefile está com o mesmo padrão.


# Importando o shapefile "Houston_City_Limit.shp"
HoustonShp <- read_sf(file.choose())

# Plotando as localizacoes dos delitos
ggplot(data = HoustonShp) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon,
                 y = lat),
             colour = 'Orange',
             size = 0.45) +
  labs(title = "Localizações dos delitos em Houston - Jan a Ago/2010")+
  theme_light()

# Plotando as localizacoes dos delitos identificando os tipos de crimes
ggplot(data = HoustonShp) +
  geom_sf(fill = "White") +
  geom_point(data = base,
             aes(x = lon,
                 y = lat,
                 colour = as.factor(offense)),
             size = 0.9) +
  guides(fill="none") +
  labs(title = "Ocorrências por crimes em Houston - Janeiro a Agosto de 2010")+
  labs(color='Crimes')+
  theme_light()

## Distribuição dos crimes por tipo
# Plotando a variável "offense" para verificar a distribuição dos tipos de crime}
ggplot(base, aes(x=as.factor(offense), fill=as.factor(offense))) +
  geom_bar() + 
  xlab("") + ylab("Ocorrências") +
  guides(fill="none") +
  scale_fill_brewer(palette="Blues") +
  labs(title = "Distribuição das ocorrências de crimes em Houston - Janeiro a Agosto de 2010")+
  coord_cartesian()

# Plotando o mapa de Houston e visualizando os eixos}
ggplot(HoustonShp) +
  geom_sf(fill = "White") +
  theme_light()

######################################
# Vamos avaliar visualmente o comportamento dos diferente tipos de crimes (offense). 
# Nessa ocasião foram escolhidos três crimes distintos para essa avaliação: rape, robbery e murder. 

# Vamos filtrar a base de dados com os crimes escolhidos: rape, robbery e murder.
dados_filtrados =  base  %>% 
  filter(offense %in% c('rape','robbery','murder')) ; dados_filtrados

# Grafico de frequencia dos crimes escolhidos
ggplot(dados_filtrados, mapping = aes(x=as.factor(offense), fill=as.factor(offense)))+
  geom_bar(width = 0.3)+
  ggtitle("Frequência das ocorrências dos crimes selecionados para o estudo")+
  theme_economist_white()+ 
  theme(aspect.ratio = .5, legend.position = "none")+
  theme(axis.title.x = element_blank())

# Plotando as localizacoes pelo tipo do delitos em mapas separados
Mapa_Houston3 = qmplot(y = lat,
                       x = lon,
                       data = dados_filtrados,
                       colour = I('orange'), 
                       size = I(1.0), 
                       darken = 0.4,
                       extent = "panel") +
  facet_wrap(~offense,
             labeller = labeller(offense = c("murder"="Murder",
                                             "rape"="Rape",
                                             "robbery"="Robbery")))+
  ggtitle(label = "Ocorrências em Houston", subtitle = "Jan a Ago/2010 - Visão desagregada") ; Mapa_Houston3

# RAPE - Separando base para avalização visual das ocorrências 
Houston_rape = filter(base, offense=="rape")

qmplot(x = lon, 
       y = lat, 
       data = Houston_rape,
       colour = I('pink'), 
       size = I(1.2), 
       darken = 0.3)+
  ggtitle(label = "Localizações das ocorrências de Rape em Houston", subtitle = "Jan a Ago/2010")

# ROBBERY - Separando base para avalização visual das ocorrências 
Houston_robbery = filter(base, offense=="robbery")

qmplot(x = lon, 
       y = lat, 
       data = Houston_robbery,
       colour = I('pink'), 
       size = I(1.2), 
       darken = 0.3)+
  ggtitle(label = "Localizações das ocorrências de Robbery em Houston", subtitle = "Jan a Ago/2010")

# MURDER - Separando base para avalização visual das ocorrências
Houston_murder = filter(base, offense=="murder")

qmplot(x = lon, 
       y = lat, 
       data = Houston_murder,
       colour = I('pink'), 
       size = I(1.2), 
       darken = 0.3)+
  ggtitle(label = "Localizações das ocorrências de Murder em Houston", subtitle = "Jan a Ago/2010")

# Criando um mapa por crime e por quadrimestre

# Criando nova variavel com o quadrimestre vs ocorrencia do crime 
dados_filtrados = dados_filtrados %>% 
  mutate(fourmounth = 
           case_when(month %in% c("january","february", "march", "april")~ "1st 2010-four-month period",
                     month %in% c("may","june","july","august")~"2nd 2010-four-month period"))

Mapa_Houston4 = qmplot(y = lat,
                       x = lon,
                       data = dados_filtrados,
                       colour = I('orange'), 
                       size = I(0.9), 
                       darken = 0.4,
                       extent = "panel") +
  facet_grid(.~offense+fourmounth, 
             labeller = labeller(offense = c("murder"="Murder",
                                             "rape" = "Rape",
                                             "robbery" = "Robbery")))+
  ggtitle(label = "Ocorrência de crimes em Houston", subtitle = "Jan a Ago/10 - Visão por quadrimestre"); Mapa_Houston4


############################################################################
# Estimando a intensidade de um padrao de pontos - Efeitos de primeira ordem  
############################################################################

# Checando o CRS de HoustonShp
st_crs(HoustonShp)

HoustonShp_32615 = st_transform(HoustonShp, 
                                crs = 32615)


HoustonO = as.owin(HoustonShp_32615)
plot(HoustonO)

# Transformando as coordenadas para coordenadas projetadas
Houston_32615 = st_transform(HoustonShp,
                             crs = 32615)

# Criando um objeto owin
HoustonO = as.owin(HoustonShp_32615)

# Plotando o objeto owin
plot(HoustonO)

# Criando o Padrao de Pontos no Plano - Planar Point Pattern
Houstonppp = ppp(x = base$lon, 
                 y = base$lat, 
                 window = HoustonO)

# Transformando os dados para o mesmo crs do shapefile
dados_4326 = st_as_sf(x = dados_filtrados,
                      coords = c("lon","lat"),
                      crs = 4326)

dados_32615 = st_transform(x = dados_4326,
                           crs = 32615)

# Comparando os objetos
head(dados_4326)
head(dados_32615)

################ Cenario 1 - Rape - Primeiro quadrimestre ################
# Filtrando a base
dados_32615_rape1 = dados_32615 %>% 
  filter(offense=='rape' & fourmounth=='1st 2010-four-month period')

# Extraindo as coordenadas do objeto dados_32615_rape1
Coordenadas_rape1 = st_coordinates(x = dados_32615_rape1)

# Criando o PPP
Houstonppp_rape1 = ppp(x = Coordenadas_rape1[,1], 
                          y = Coordenadas_rape1[,2], 
                          window = HoustonO)

# Estimando o raio
raio_est_rape1 = bw.diggle(Houstonppp_rape1)
raio_est_rape1

# Estimando a intensidade com kernel Gaussiano
rape1_kg = density.ppp(x = Houstonppp_rape1,
                       sigma = raio_est_rape1,
                       kernel = "gaussian") 

rape1_kg_grafico = plot(rape1_kg, 
                        main="Rape - Intensidade estimada com Kernel Gaussiano - Primeiro quadrimestre/10", 
                        cex.main=0.5)

# Plotando as ocorrencias de rapes no primeiro quadrimestre para niveis de comparacao 
# com a estimacao feita pelo Kernel Gaussiano
Ocorrencia_rape1 = plot(Houstonppp_rape1,
                        pch=21, 
                        cex=0.9, 
                        bg="blue", 
                        main="Rape - Ocorrências no primeiro quadrimestre/2010")

################ Cenario 2 - Rape - Segundo quadrimestre ################
# Filtrando a base
dados_32615_rape2 = dados_32615 %>% 
  filter(offense=='rape' & fourmounth=='2nd 2010-four-month period')

# Extraindo as coordenadas do objeto dados_32615_rape2
Coordenadas_rape2 = st_coordinates(x = dados_32615_rape2)

# Criando o PPP
Houstonppp_rape2 = ppp(x = Coordenadas_rape2[,1], 
                          y = Coordenadas_rape2[,2], 
                          window = HoustonO)
#Estimando o raio
raio_est_rape2 = bw.diggle(Houstonppp_rape2)
raio_est_rape2

#Estimando a intensidade com kernel Gaussiano
rape2_kg = density.ppp(x = Houstonppp_rape2,
                       sigma = raio_est_rape2,
                       kernel = "gaussian") 

rape2_kg_grafico = plot(rape2_kg, 
                        main="Rape - Intensidade estimada com Kernel Gaussiano - Segundo quadrimestre/10", 
                        cex.main=0.5)

# Plotando as ocorrencias de rapes no segundo quadrimestre para niveis de comparacao 
# com a estimacao feita pelo Kernel Gaussiano
Ocorrencia_rape2 = plot(Houstonppp_rape2,
                        pch=21, 
                        cex=0.9, 
                        bg="blue", 
                        main="Rape - Ocorrências no segundo quadrimestre/2010")

################ Cenario 1 - Robbery - Primeiro quadrimestre ################
# Filtrando a base

dados_32615_robbery1 = dados_32615 %>% 
  filter(offense=='robbery' & fourmounth=='1st 2010-four-month period')

# Extraindo as coordenadas do objeto dados_32615_robbery1
Coordenadas_robbery1 = st_coordinates(x = dados_32615_robbery1)

# Criando o PPP
Houstonppp_robbery1 = ppp(x = Coordenadas_robbery1[,1], 
                        y = Coordenadas_robbery1[,2], 
                        window = HoustonO)
# Estimando o raio
raio_est_robbery1 = bw.diggle(Houstonppp_robbery1)
raio_est_robbery1

#Estimando a intensidade com kernel Gaussiano
robbery1_kg = density.ppp(x = Houstonppp_robbery1,
                          sigma = raio_est_robbery1,
                          kernel = "gaussian") 

robbery1_kg_grafico = plot(robbery1_kg, 
                           main="Robbery - Intensidade estimada com Kernel Gaussiano - Primeiro quadrimestre/10", 
                           cex.main=0.5)

# Plotando as ocorrencias de robbery no primeiro quadrimestre para niveis de comparacao 
# com a estimacao feita pelo Kernel Gaussiano
Ocorrencia_robbery1 = plot(Houstonppp_robbery1,
                           pch=21, 
                           cex=0.9, 
                           bg="blue", 
                           main="Robbery - Ocorrências no primeiro quadrimestre/2010")

################ Cenario 2 - Robbery - Segundo quadrimestre ################
# Filtrando a base
dados_32615_robbery2 = dados_32615 %>% 
  filter(offense=='robbery' & fourmounth=='2nd 2010-four-month period')

#Extraindo as coordenadas do objeto dados_32615_robbery2
Coordenadas_robbery2 = st_coordinates(x = dados_32615_robbery2)

#criando o PPP
Houstonppp_robbery2 = ppp(x = Coordenadas_robbery2[,1], 
                        y = Coordenadas_robbery2[,2], 
                        window = HoustonO)
#Estimando o raio
raio_est_robbery2 = bw.diggle(Houstonppp_robbery2)
raio_est_robbery2

#Estimando a intensidade com kernel Gaussiano
robbery2_kg = density.ppp(x = Houstonppp_robbery2,
                          sigma = raio_est_robbery2,
                          kernel = "gaussian") 

robbery2_kg_grafico = plot(robbery2_kg, 
                           main="Robbery - Intensidade estimada com Kernel Gaussiano - Segundo quadrimestre/10", 
                           cex.main=0.5)

# Plotando as ocorrencias de robbery no segundo quadrimestre para niveis de comparacao 
# com a estimacao feita pelo Kernel Gaussiano
Ocorrencia_robbery2 = plot(Houstonppp_robbery2,
                           pch=21, 
                           cex=0.9, 
                           bg="blue", 
                           main="Robbery - Ocorrências no segundo quadrimestre/2010")

################ Cenario 1 - Murder - Primeiro quadrimestre ################
# Filtrando a base
dados_32615_murder1 = dados_32615 %>% 
  filter(offense=='murder' & fourmounth=='1st 2010-four-month period')

# Extraindo as coordenadas do objeto dados_32615_murder1
coordenadas_murder1 = st_coordinates(x = dados_32615_murder1)

# Criando o PPP
Houstonppp_murder1 = ppp(x = coordenadas_murder1[,1], 
                         y = coordenadas_murder1[,2], 
                         window = HoustonO)
# Estimando o raio
raio_est_murder1 = bw.diggle(Houstonppp_murder1)
raio_est_murder1

#Estimando a intensidade com kernel Gaussiano
murder1_kg = density.ppp(x = Houstonppp_murder1,
                         sigma = raio_est_murder1,
                         kernel = "gaussian") 

murder1_kg_grafico = plot(murder1_kg, 
                          main="Murder - Intensidade estimada com Kernel Gaussiano - Primeiro quadrimestre/10", 
                          cex.main=0.5)

# Plotando as ocorrencias de assassinatos no primeiro quadrimestre para niveis de comparacao 
# com a estimacao feita pelo Kernel Gaussiano
Ocorrencia_murder1 = plot(Houstonppp_murder1, 
                          pch=21, 
                          cex=0.9, 
                          bg="blue", 
                          main="Murder - Ocorrências no primeiro quadrimestre/2010")

################ Cenario 2 - Murder - Segundo quadrimestre ################
# Filtrando a base
dados_32615_murder2 = dados_32615 %>% 
  filter(offense=='murder' & fourmounth=='2nd 2010-four-month period')

# Extraindo as coordenadas do objeto dados_32615_murder2
Coordenadas_murder2 = st_coordinates(x = dados_32615_murder2)

# Criando o PPP
Houstonppp_murder2 = ppp(x = Coordenadas_murder2[,1], 
                         y = Coordenadas_murder2[,2], 
                         window = HoustonO)

# Estimando o raio
raio_est_murder2 = bw.diggle(Houstonppp_murder2)
raio_est_murder2

# Estimando a intensidade com kernel Gaussiano
murder2_kg = density.ppp(x = Houstonppp_murder2,
                         sigma = raio_est_murder2,
                         kernel = "gaussian") 

murder2_kg_grafico = plot(murder2_kg, 
                          main="Murder - Intensidade estimada com Kernel Gaussiano - Segundo quadrimestre/10", 
                          cex.main=0.5)

# Plotando as ocorrencias de assassinatos no segundo quadrimestre para niveis de comparacao 
# com a estimacao feita pelo Kernel Gaussiano
Ocorrencia_murder2 = plot(Houstonppp_murder2, 
                          pch=21, 
                          cex=0.9, 
                          bg="blue", 
                          main="Murder - Ocorrências no segundo quadrimestre/2010")


############################################################################
# Estimando a intensidade de um padrao de pontos - Efeitos de segunda ordem  
############################################################################

################ Cenario 1 - Rape - Primeiro quadrimestre ################
# Estimando a funcao G
HoustonRape1_G = Gest(Houstonppp_rape1)

#Plotando a funcao G
plot(HoustonRape1_G, main="Função G - Rapes - Primeiro quadrimestre/10")

################ Cenario 2 - Rape - Segundo quadrimestre ################
# Estimando a funcao G
HoustonRape2_G = Gest(Houstonppp_rape2)

# Plotando a funcao G
plot(HoustonRape2_G, main="Função G - Rapes - Segundo quadrimestre/10")

################ Cenario 1 - Robbery - Primeiro quadrimestre ################
# Estimando a funcao G
HoustonRobbery1_G = Gest(Houstonppp_robbery1)

# Plotando a funcao G
plot(HoustonRobbery1_G, main="Função G - Robbery - Primeiro quadrimestre/10")

################ Cenario 2 - Robbery - Segundo quadrimestre ################
# Estimando a funcao G
HoustonRobbery2_G = Gest(Houstonppp_robbery2)

# Plotando a funcao G
plot(HoustonRobbery2_G, main="Função G - Robbery - Segundo quadrimestre/10")

################ Cenario 1 - Murder - Primeiro quadrimestre ################
# Estimando a funcao G
HoustonMurder1_G = Gest(Houstonppp_murder1)

# Plotando a funcao G
plot(HoustonMurder1_G, main="Função G - Murder - Primeiro quadrimestre/10")

################ Cenario 2 - Murder - Segundo quadrimestre ################
# Estimando a funcao G
HoustonMurder2_G = Gest(Houstonppp_murder2)

# Plotando a funcao G
plot(HoustonMurder2_G, main="Função G - Murder - Segundo quadrimestre/10")


par(mfrow=c(2,3))
par(mar=c(0.5,0.5,1.5,0.5,1.5,0.5))
plot(HoustonRape1_G, pch=21, cex=1, bg="blue", x.lab = 1, cex.main=1.5, main="Função G - Rape - Primeiro quadrimestre/10")
plot(HoustonRape2_G, pch=21, cex=1, bg="blue", x.lab = 1, cex.main=1.5, main="Função G - Rape - Segundo quadrimestre/10")
plot(HoustonRobbery1_G, pch=21, cex=1, bg="blue", x.lab = 1, cex.main=1.5, main="Função G - Robbery- Primeiro quadrimestre/10")
plot(HoustonRobbery2_G,pch=21, cex=1, bg="blue", x.lab = 1, cex.main=1.5, main="Função G - Robbery - Primeiro quadrimestre/10")
plot(HoustonMurder1_G, pch=21, cex=1, bg="blue", x.lab = 1, cex.main=1.5, main="Função G - Murder - Primeiro quadrimestre/10")
plot(HoustonMurder2_G, pch=21, cex=1, bg="blue", x.lab = 1, cex.main=1.5, main="Função G - Murder - Segundo quadrimestre/10")
par(mfrow=c(1,1))

############################################################################
# Padrao de pontos - Teste formal - Clark-Evans 
############################################################################

# H0: o padrao de pontos possui CRS (cenario de completa aleatoriedade espacial)
# H1: o padrao de pontos eh agrupado (optei por esta hipotese alternativa pois eh o que os graficos da funcao G indicam)

# Quando H1 nao for aceita, testaremos a hipotese de repulsao por via das duvidas mas muito provavelmente neste caso 
# Tambem nao aceitaremos H1, ja que em nenhum dos casos a analise explorataria mostrou que poderia haver padrao de repulsao


################ Cenario 1 - Rape - Primeiro quadrimestre ################

clarkevans.test(X = Houstonppp_rape1, 
                alternative = "clustered") 
# p-valor < 0.05: o padrao eh de agrupamento com 95% de confianca, confirmando 
# o que a analise exploratoria indicou


################ Cenario 2 - Rape - Segundo quadrimestre ################

clarkevans.test(X = Houstonppp_rape2, 
                alternative = "clustered") 
# p-valor < 0.05: o padrao eh de agrupamento com 95% de confianca, confirmando 
# o que a analise exploratoria indicou

################ Cenario 1 - Robbery - Primeiro quadrimestre ################

clarkevans.test(X = Houstonppp_robbery1, 
                alternative = "clustered") 
# p-valor < 0.05: o padrao eh de agrupamento com 95% de confianca, confirmando 
# o que a analise exploratoria indicou

################ Cenario 2 - Robbery - Segundo quadrimestre ################

clarkevans.test(X = Houstonppp_robbery2, 
                alternative = "clustered") 
# p-valor < 0.05: o padrao eh de agrupamento com 95% de confianca, confirmando 
# o que a analise exploratoria indicou

################ Cenario 1 - Murder - Primeiro quadrimestre ################

clarkevans.test(X = Houstonppp_murder1, 
                alternative = "clustered") 
# p-valor < 0.05: o padrao eh de agrupamento com 95% de confian?a, confirmando
# o que a analise exploratoria indicou

################ Cenario 2 - Murder - Segundo quadrimestre ################

clarkevans.test(X = Houstonppp_murder2, 
                alternative = "clustered") 

clarkevans.test(X = Houstonppp_murder2, 
                alternative = "regular")

# p-valor > 0.05: o padrao eh de completa aleatoriedade espacial
# com 95% de confianca
