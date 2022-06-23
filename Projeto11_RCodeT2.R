### Análise Preditiva - FGV 
### T2: IDC - Email Marketing
### Autor: Rafael Rocha

## Instalando os pacotes 
library(tidyverse)
library(dplyr)
library(zoo)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggfortify)
library(patchwork)
library(tree)
library(rpart)
library(rpart.plot)
library(mlbench)

## Importando e carregando os dados

# Dataset com todas as informacoes sobre campanhas ja realizadas e sobre os clientes
df <- read.csv2('/Users/rafaeldesouza/Desktop/AnalisePreditiva/email_marketing.csv', sep = ',', dec = '.')

df %>% glimpse()


