################################################################
# Analise Preditiva Avancada - Avaliacao Final - T8"
# "Rafael Rocha - A56660250"
# "Abril/2021"
################################################################

################################################################
#            Modelo preditivo do valor de vendas
################################################################

# Carregando os pacotes
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(mlbench)
library(readxl)
library(stringr)
library(lubridate)
library(readr)
library(DataExplorer)
library(tidymodels)
library(patchwork)
library(heatmaply)
library(reactable)
library(pROC)
library(ROCR)
library(kableExtra)
library(knitr)

options(scipen=7)
options(warn=-1)

#############################################
################## RETAIL ###################
#############################################

########### Importando o dataset ############

retail <- read.csv(file="/Users/rafaeldesouza/Desktop/Analise Pred Avancada/Projeto Final/Retail.csv")

head(retail)
summary(retail)

str(retail)

introduce(retail)
create_report(retail)

#Se nota uma falta de dados
#É possível notar que há falta de dados a partir do final de 2012 e no início de 2010 para a feature Weekly_Sales.
#Há sazonalidade e os volumes totais de vendas (Weekly_Sales) não parecem ter tendência de crescimento ao longo dos anos.
#A feature Weekly_Sales deveria ser numerica e não é pois temos valores #N/D. Nenhuma loja tem dados a partir de 2012-11-02 e temos registros até 2013-07-26.
#As features MarkDown passam a ter dados somente após 2011/11/11.
# Ao inves de substituir por 0 ou por media ou mediana decidi pela retirada dos dados faltantes

###########################################
################ Ajustes ##################

retail <- retail %>% 
  mutate(Date = lubridate::dmy(Date)) %>% 
  mutate(Weekly_Sales = as.numeric(Weekly_Sales)) 

n_row_antes <- nrow(retail)
retail <- retail %>% 
  filter(!is.na(Weekly_Sales),
         Date>lubridate::ymd("2011-11-11"))

# Checando dados pos limpeza
plot_intro(retail)

#Após fazer os ajustes a base deixa de ter dados faltando ficando pronta para iniciar os modelos

retail %>% 
  select_if(is.numeric) %>% 
  gather(key, value, -Weekly_Sales) %>% 
  ggplot(aes(x = value, y = Weekly_Sales))+
  geom_point()+
  facet_wrap(~key, scales = "free", nrow = 2)+
  geom_smooth()+
  labs(x="")


retail %>% 
  na.omit() %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  round(digits = 4) %>% 
  heatmaply::heatmaply_cor(
    xlab = "Features",
    ylab = "Features",
    k_col = 2,
    k_row = 4
  )

#################################################################
############# Separando base para treino e teste ################

# Separando 70% da base para treino e 30% para teste 
set.seed(314)

retail_cut <- initial_split(data = retail, strata = Weekly_Sales, prop = 0.7)

retail_train <- training(retail_cut)
retail_test  <- testing(retail_cut)

# Centralizando a target na média e dividindo pelo desvio padrão para normalizar
m <- mean(retail_train$Weekly_Sales)
s <- sd(retail_train$Weekly_Sales)

center_scale <- function(x, m, s){
  (x - m) / s
}

retail_train$Weekly_Sales <- center_scale(retail_train$Weekly_Sales, m, s)
retail_test$Weekly_Sales <- center_scale(retail_test$Weekly_Sales, m, s)

# Preparando a formula para a modelagem com os filtros e considerando os feriados
retail_recipe <- recipe(Weekly_Sales ~ ., data = retail) %>% 
  step_mutate(
    wday = lubridate::wday(Date),
    month = lubridate::month(Date),
    quarter = lubridate::quarter(Date),
    IsHoliday = as.numeric(IsHoliday),
    Store = as.numeric(Store)) %>% 
  step_center(all_predictors(), -wday, -month, -quarter, -IsHoliday, -Date, na_rm = T) %>%
  step_scale(all_predictors(), -wday, -month, -quarter, -IsHoliday, -Date, na_rm = T) %>%
  step_knnimpute(MarkDown1, MarkDown2, MarkDown3, MarkDown4, MarkDown5, neighbors = 5) %>% 
  step_rm(Date)

str(retail_recipe)

#################################################################
############## Testagem de modelos de regressao #################

########### Regressao Linear FELIPE
linear_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression") 

linear_wflow <- workflow() %>% 
  add_recipe(retail_recipe) %>% 
  add_model(linear_model)

linear_fit <- linear_wflow %>% fit(retail_train) 
linear_fit

linear_pred <- predict(linear_fit, new_data = retail_test)

linear_r2 <- rsq_vec(truth = retail_test$Weekly_Sales, 
                     estimate = linear_pred$.pred)

linear_r2

## Fazendo Scoring do Modelo
retail_test$Model <- linear_pred
head(retail_test)

############ REG LINEAR RAFA
set.seed(314)
model_lm <- train(Weekly_Sales~., data = retail_train, method = "lm", na.action=na.exclude)
model_lm
 
# Utilizando a função VarImp do mlbench e plotando sua tabela vemos em oredem as features mais importantes do modelos
imp_lm <- varImp(model_lm, useModel=FALSE, scale=FALSE)
imp_lm
plot(imp_lm)

# matriz de confusao 
pred_lm <- predict(model_lm, newdata= retail_test)
head(pred_lm)

pred_lm <- as.factor(pred_lm)
#confusionMatrix(data=as.factor(pred_lm), as.factor(retail_test$Weekly_Sales))

# Agora com o modelo treinado podemos fazer o "scoring" nos dados que haviam sindo separados para teste e quaisquer outros que o modelo ainda não tenha visto.
y_Model <- predict(model_lm, retail_test)
retail_test$Model <- y_Model
head(retail_test)

# Armazenando os indicadores
pred2_lm <- predict(model_lm, retail_test)
RMSE_lm <- round(rmse_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_lm)), 2)
MAPE_lm <- round(mape_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_lm)), 2)
R2_lm <- round(rsq_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_lm)), 2)
ACCURACY_lm <- round(accuracy_vec(retail_test$Weekly_Sales, pred2_lm),2)


############################## 
##### Arvore de Decisao de Regressao ###### FELIPE
tree_model <- decision_tree(
  cost_complexity = 0.01, # cp
  tree_depth = 30,        # maxdepth
  min_n = 20              # minsplit
) %>% 
  set_engine("rpart") %>%
  set_mode("regression")

tree_wflow <-
  workflow() %>% 
  add_recipe(retail_recipe) %>% 
  add_model(tree_model)

tree_fit <- tree_wflow %>% fit(retail_train) 

tree_pred <- predict(tree_fit, new_data = retail_test)

tree_r2 <- rsq_vec(truth = retail_test$Weekly_Sales, 
                   estimate = tree_pred$.pred)

tree_r2

## Montando a Matriz de Confusão
confusionMatrix(data=tree_pred, retail_test$Weekly_Sales)

## Scoring arvore de decisao
retail_test$Model <- tree_pred
head(retail_test)

############ ARVORE DE DECISAO DE REGRESSAO (RODANDO) - RAFA
set.seed(314)

library(rpart)

model_arvore <- rpart(Weekly_Sales~., method = "anova", data=retail_train)

model_arvore
printcp(model_arvore) # display the results
plotcp(model_arvore) # visualize cross-validation results
summary(model_arvore) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(model_arvore) # visualize cross-validation results

# testando modelo
pred_arvore <- predict(model_arvore, newdata= retail_test)
head(pred_arvore)

# Armazenando os indicadores
pred2_arvore <- predict(model_arvore, retail_test)
RMSE_arvore <- round(rmse_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_arvore)), 2)
MAPE_arvore <- round(mape_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_arvore)), 2)
R2_arvore <- round(rsq_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_arvore)), 2)
RMSE_arvore
MAPE_arvore
R2_arvore

##########################
##### Redes Neurais ######
nnet_model <-
  mlp(epochs = 100, # maxit
      hidden_units = 32, # size = 5
      penalty = 0.4 # decay
  ) %>%
  set_mode("regression") %>% 
  set_engine("nnet", verbose = 0)

nnet_wflow <-
  workflow() %>% 
  add_recipe(retail_recipe) %>% 
  add_model(nnet_model)

nnet_fit <- nnet_wflow %>% fit(retail_train) 
nnet_fit

nnet_pred <- predict(nnet_fit, new_data = retail_test)

nnet_r2 <- rsq_vec(truth = retail_test$Weekly_Sales, 
                   estimate = nnet_pred$.pred)
nnet_r2

## Scoring redes neurais
retail_test$Model <- nnet_pred
head(retail_test)

# Armazenando os indicadores
pred2_nnet <- predict(nnet_fit, retail_test)
RMSE2_nnet <- round(rmse_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_nnet)), 2)
MAPE2_nnet <- round(mape_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_nnet)), 2)
R2_2_nnet <- round(rsq_vec(as.numeric(retail_test$Weekly_Sales), as.numeric(pred2_nnet)), 2)
ACCURACY2_nnet <- round(accuracy_vec(retail_test$Weekly_Sales, pred2_nnet),2)





perf_mod2 <- tibble(
  Modelos = c("Regressão Linear", "Arvore de Decisao", "Redes Neurais"),
  R2 = c("30", "62", "10") %>% 
    paste0("%"),
) 

resultado <- perf_mod2 %>% 
  kable(caption = "Performance dos Modelos") %>%
  kable_styling("striped", full_width = F)
resultado 

