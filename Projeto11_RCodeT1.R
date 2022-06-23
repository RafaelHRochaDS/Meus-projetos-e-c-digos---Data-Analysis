### Análise Preditiva - FGV 
### T1: B2W LABs - Pricing Challenge
### Autor: Rafael Rocha - A56660250

## Instalando os pacotes 
library(tidyverse)
library(dplyr)
library(zoo)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggfortify)
library(patchwork)
library(caTools) 
library(MASS)

## Importando e carregando os dados
# Dataset principal com dados de vendas
sales <- read.csv2('/Users/rafaeldesouza/Desktop/AnalisePreditiva/test_prc/sales.csv', sep = ',', dec = '.')

# Dataset com informações dos competidores
prices <- read.csv2('/Users/rafaeldesouza/Desktop/AnalisePreditiva/test_prc/comp_prices.csv', sep = ',', dec = '.')

# Tratamento dos dados -------------------------------------------
# Verificando as classes das variáveis
sapply(prices, class)
sapply(sales, class)

# Alterando classes para "Date"
sales$DATE_ORDER <- sales$DATE_ORDER%>%as.Date()
prices$DATE_EXTRACTION <- prices$DATE_EXTRACTION%>%as.Date()

# Padronizando as variáveis de data para mês e ano
sales$MONTH_YEAR <- as.yearmon(sales$DATE_ORDER, "%m/%Y")
prices$MONTH_YEAR <- as.yearmon(prices$DATE_EXTRACTION, "%m/%Y")

# Checando ausência de informações
sales$DATE_ORDER <- NULL
prices$DATE_EXTRACTION <- NULL

# Conferir premissa que existem 2 registros por dia:
prices %>% count(PROD_ID, DATE_EXTRACTION, COMPETITOR, PAY_TYPE, sort = T)

# Aparentemente existem mais do que 2 registros por dia em alguns casos

# Criando variável PRICE que receberá o preço unitário de cada produto
sales <-  aggregate(sales[,c('QTY_ORDER','REVENUE')],
                    by = list(PROD_ID = sales$PROD_ID, MONTH_YEAR = sales$MONTH_YEAR),
                    FUN = sum)

sales$PRICE  <- sales$REVENUE/sales$QTY_ORDER

# Selecionando variáveis para otimizar a base de sales levando em consideração o problema
sales%>%
  select(-QTY_ORDER)%>%
  spread(PROD_ID,REVENUE)%>%
  select(-MONTH_YEAR)%>%
  sapply( function(x) ifelse(length(boxplot(x,plot=FALSE)['out'][[1]])==1,
                             boxplot(x,plot=FALSE)['out'],0))

# Preparando base dos competidores
prices$COMPETITOR_PAY_TYPE <- paste(prices$COMPETITOR,'_',prices$PAY_TYPE,sep= '')

# Checando ausência de informações
prices$COMPETITOR <- NULL
prices$PAY_TYPE <- NULL

# Selecionando variáveis para otimizar a base dos competidores levando em consideração o problema
prices <-  aggregate(prices[,'COMPETITOR_PRICE'],
                     by = list(PROD_ID = prices$PROD_ID, MONTH_YEAR = prices$MONTH_YEAR, 
                               COMPETITOR_PAY_TYPE = prices$COMPETITOR_PAY_TYPE),
                     FUN = mean)

# Redefinindo as variáveis mais relevantes para a solução do problema
colnames(prices) <- c("PROD_ID","MONTH_YEAR","COMPETITOR_PAY_TYPE","COMPETITOR_PRICE")

# Colocando os competidores apresentados em colunas
prices <- spread(prices,COMPETITOR_PAY_TYPE,COMPETITOR_PRICE)

# Unindo variáveis em uma nova tabela 
df_total_sales <- left_join(sales, prices, by = c('PROD_ID','MONTH_YEAR'))

# Gerando médias de preços 
df_total_sales$price_ind_1 <- df_total_sales[,grepl('1',colnames(df_total_sales))]%>%apply(1,function(x){
  x <- x[!is.na(x)] 
  mean(x)
})

df_total_sales$price_ind_2 <- df_total_sales[,grepl('2',colnames(df_total_sales))]%>%apply(1,function(x){
  x <- x[!is.na(x)] 
  mean(x)
})

# Notamos que algum competidores nao vendem alguns produtos pois nao receberam nenhum registro 
# Vamos substitui-los por zero
df_total_sales[is.na(df_total_sales)] <- 0

# Criando variáveis com as médias
df_total_sales$price_ind_mean <- df_total_sales[,c('price_ind_1','price_ind_2')]%>%apply(1,mean)

df_total_sales$price_index <- df_total_sales$PRICE/df_total_sales$price_ind_mean
df_total_sales$price_index_1 <- df_total_sales$PRICE/df_total_sales$price_ind_1
df_total_sales$price_index_2 <- df_total_sales$PRICE/df_total_sales$price_ind_2
df_total_sales[is.infinite(df_total_sales$price_index),'price_index'] <- 0

# Modelagem -------------------------------------------
modelo <- list()
for (i in unique(df_total_sales$PROD_ID)) {
  modelo_dt <- df_total_sales[df_total_sales$PROD_ID==i,c('MONTH_YEAR','QTY_ORDER','price_index')]
  modelo_dt$trend <- seq(1,NROW(modelo_dt))
  modelo_dt$MONTH_YEAR <- NULL
  train <- modelo_dt[1:NROW(modelo_dt)*0.6,]
  test <- modelo_dt[(NROW(modelo_dt)*0.6+1):NROW(modelo_dt),]
  mod1 <- lm(QTY_ORDER~.,train)
  step.mod1 <- stepAIC(mod1, direction = "both", 
                          trace = FALSE)
  summary(mod1)
  summary(step.mod1)
  
  mape <- mean(abs((test$QTY_ORDER-(predict(mod1, test, se.fit = TRUE)$fit))/test$QTY_ORDER))
  
  if(!exists('df_mod')){
    df_mod <- data.frame('PROD_ID'=i,'INTERCEPT'=mod1$coefficients[1][[1]],'Price_index'=mod1$coefficients[2][[1]],
                            'trend'= mod1$coefficients[3][[1]],'Mape' = mape)
  }else{
    df_mod2 <- data.frame('PROD_ID'=i,'INTERCEPT'=mod1$coefficients[1][[1]],'Price_index'=mod1$coefficients[2][[1]],
                             'trend'= mod1$coefficients[3][[1]],'Mape' = mape)
    df_mod <- rbind(df_mod,df_mod2)
  }
}
