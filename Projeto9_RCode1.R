################################################################
# Analise Preditiva Avancada - Avaliacao Final - T8"
# "Rafael Rocha - A56660250"
# "Abril/2021"
################################################################

################################################################
#                 Targeted Advertising
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

###########################################
############### MARKETING #################
###########################################

########### Importando o dataset ############
marketing <- read.csv(file="/Users/rafaeldesouza/Desktop/Analise Pred Avancada/Projeto Final/Marketing.csv", header=TRUE, sep=";")

head(marketing)
summary(marketing)

str(marketing)

introduce(marketing)
create_report(marketing)

plot_correlation(marketing)

###########################################
########### Ajustes base ##################
marketing$CONS_CONF_IDX <- as.numeric(str_replace(marketing$CONS_CONF_IDX, "_", ".")) * (-1)

# Setando ajustes 
ajustes <- function(x){
  x %>% 
    mutate(SUBSCRIBED = factor(SUBSCRIBED, levels = c("yes", "no"))) %>% 
    mutate_at(c("EMP_VAR_RATE", "CONS_CONF_IDX"),
              ~as.numeric(str_replace_all(.x, "_", ".") ) ) %>%
    mutate(MONTH = case_when(
      MONTH=="jan"~1, MONTH=="feb"~2, MONTH=="mar"~3, MONTH=="apr"~4, MONTH=="may"~5, MONTH=="jun"~6, MONTH=="jul"~7,
      MONTH=="aug"~8, MONTH=="sep"~9, MONTH=="oct"~10, MONTH=="nov"~11, MONTH=="dec"~12)) %>% 
    mutate(DAY_OF_WEEK = case_when(
      DAY_OF_WEEK == "mon"~1, DAY_OF_WEEK == "tue"~2, DAY_OF_WEEK == "wed"~3, DAY_OF_WEEK == "thu"~4,
      DAY_OF_WEEK == "fri"~5, DAY_OF_WEEK == "sat"~6, DAY_OF_WEEK == "sun"~7 )) %>% 
    mutate(PDAYS = ifelse(PDAYS==-1, "no", "yes")) %>% 
    mutate(JOB = str_replace_all(JOB, "-", "_")) %>% 
    filter(AGE<60) %>% 
    mutate(DEFAULT = factor(DEFAULT, levels = c("no", "unknown", "yes")))
}

# Aplicando ajustes na base
marketing <- ajustes(marketing)

# Checando base com ajustes
introduce(marketing)
create_report(marketing, y = "SUBSCRIBED")

#################################################################
############# Separando base para treino e teste ################
set.seed(314)

## Separando 70% da base para treino e 30% para teste, como sugerido.
marketing_cut <- initial_split(data = marketing, strata = SUBSCRIBED, prop = 0.7)

marketing_train <- training(marketing_cut)
marketing_test  <- testing(marketing_cut)

#################################################################
############## Teste de modelos de classificacao ################

## Regressao Logistica ## 
set.seed(314)
options(warn=-1)

cv <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE,
                   summaryFunction=twoClassSummary, classProbs = TRUE)


model_glm <- train(SUBSCRIBED~., data = marketing_train, method = "glm", 
               metric="ROC",trControl = cv, control = list(maxit = 50))

model_glm

dfPred_glm <- predict(model_glm, newdata=marketing_test)

#Adicionando matriz de confusão para validação do modelo logístico.
confusionMatrix(data=dfPred_glm, marketing_test$SUBSCRIBED)

#Agora podemos fazer o score do modelo na base de teste e obter as probabilidades de cada classe de acordo com o modelo
dfProbs_glm <- predict(model_glm, newdata=marketing_test, type="prob")
head(dfProbs_glm)

# Verificando Importância das Variáveis Preditoras
imp_glm <- varImp(model_glm, scale=FALSE)
imp_glm
plot(imp_glm)

# Armazenando os indicadores
pred_glm <- predict(model_glm, marketing_test)
RMSE_glm <- round(rmse_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_glm)), 2)
MAPE_glm <- round(mape_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_glm)), 2)
R2_glm <- round(rsq_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_glm)), 2)
ACCURACY_glm <- round(accuracy_vec(marketing_test$SUBSCRIBED, pred_glm),2)

############################### 
##### Arvores de Decisao ###### 
# Optei por rodar apenas dois metodos devido ao custo computacional

###############################
##### Arvore Simples ######
set.seed(314)

# Definindo Parâmetros do Cross Validation
cv_arvoresimples <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)

# Treinando Modelo de Árvore Simples
model_arvoresimples <- train(SUBSCRIBED~. , data = marketing_train, method = "C5.0Tree",trControl = cv_arvoresimples)

model_arvoresimples

# Montando a Matriz de Confusão
pred_arvoresimples <- predict(model_arvoresimples ,newdata=marketing_test)
confusionMatrix(data=pred_arvoresimples, marketing_test$SUBSCRIBED)

# Fazendo Scoring do Modelo
classProbs_arvoresimples <- predict(model_arvoresimples, newdata=marketing_test, type="prob")
head(classProbs_arvoresimples)

# Verificando Importância das Variáveis Preditoras
imp_arvoresimples <- varImp(model_arvoresimples, scale=FALSE)
imp_arvoresimples
plot(imp_arvoresimples)

# Armazenando os indicadores
pred_arvoresimples <- predict(model_arvoresimples, marketing_test)
RMSE_arvoresimples <- round(rmse_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_arvoresimples)), 2)
MAPE_arvoresimples <- round(mape_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_arvoresimples)), 2)
R2_arvoresimples <- round(rsq_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_arvoresimples)), 2)
ACCURACY_arvoresimples <- round(accuracy_vec(marketing_test$SUBSCRIBED, pred_arvoresimples),2)

###############################
##### Arvore com Boosting ######
set.seed(314)

# Definindo Parâmetros do Cross Validation
cv_arvoreboosting <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE, classProbs=TRUE)

# Treinando Modelo com Boosting (XgBoost)
model_arvoreboosting <- train(SUBSCRIBED~. , data = marketing_train, method = "xgbTree",trControl = cv_arvoreboosting)

model_arvoreboosting

# Gerando Matriz de Confusão
pred_arvoreboosting <- predict(model_arvoreboosting ,newdata=marketing_test)
confusionMatrix(data=pred_arvoreboosting, marketing_test$SUBSCRIBED)

# Fazendo Scoring do Modelo
classProbs_arvoreboosting <- predict(model_arvoreboosting, newdata=marketing_test, type="prob")
head(classProbs_arvoreboosting)

# Importância das Variáveis Preditoras
imp_arvoreboosting <- varImp(model_arvoreboosting, scale=FALSE)
imp_arvoreboosting
plot(imp_arvoreboosting)

# Armazenando os indicadores
pred_arvoreboosting <- predict(model_arvoreboosting, marketing_test)
RMSE_arvoreboosting <- round(rmse_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_arvoreboosting)), 2)
MAPE_arvoreboosting <- round(mape_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_arvoreboosting)), 2)
R2_arvoreboosting <- round(rsq_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_arvoreboosting)), 2)
ACCURACY_arvoreboosting <- round(accuracy_vec(marketing_test$SUBSCRIBED, pred_arvoreboosting),2)

################################
############# SVM ############## 

###############################
##### SVM - Linear Kernel #####
options(warn=-1)

# Definindo Parâmetros do Cross Validation
cv_svmlinear <- trainControl(method = "repeatedcv", number = 10)

# Treinando Modelo
model_svmlinear <- train(SUBSCRIBED~., data = marketing_train, method = "svmLinear", trControl = cv_svmlinear, preProcess = c("center", "scale"))

model_svmlinear

# Gerando Matriz de Confusão
dfPred_svmlinear <- predict(model_svmlinear, newdata=marketing_test)
confusionMatrix(data=dfPred_svmlinear, marketing_test$SUBSCRIBED)

# Armazenando os indicadores
pred_svmlinear <- predict(model_svmlinear, marketing_test)
RMSE_svmlinear <- round(rmse_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_svmlinear)), 2)
MAPE_svmlinear <- round(mape_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_svmlinear)), 2)
R2_svmlinear <- round(rsq_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_svmlinear)), 2)
ACCURACY_svmlinear <- round(accuracy_vec(marketing_test$SUBSCRIBED, pred_svmlinear),2)

######### NAO COLOCAR DEVIDO AO CUSTO COMPUTACIONAL ###################
##### SVM - RBF Kernel #####
options(warn=-1)

# Definindo Parâmetros do Cross Validation
cv_svmrbf <- trainControl(method = "repeatedcv", number = 10)

# Treinando Modelo
model_svmrbf <- train(SUBSCRIBED~., data = marketing_train, method = "svmRadial", trControl = cv_svmrbf, 
                 preProcess = c("center", "scale"))

model_svmrbf

# Gerando Matriz de Confusão
dfPred_svmrbf <- predict(model_svmrbf, newdata=marketing_test)
confusionMatrix(data=dfPred_svmrbf, marketing_test$SUBSCRIBED)

# Armazenando os indicadores
pred_svmrbf <- predict(model_svmrbf, marketing_test)
RMSE_svmrbf <- round(rmse_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_svmrbf)), 2)
MAPE_svmrbf <- round(mape_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_svmrbf)), 2)
R2_svmrbf <- round(rsq_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_svmrbf)), 2)
ACCURACY_svmrbf <- round(accuracy_vec(marketing_test$SUBSCRIBED, pred_svmrbf),2)

##########################################
############# Redes Neurais ############## 
set.seed(314)

# Treinando Modelo
model_nnet <- train(SUBSCRIBED~., data = marketing_train, method='nnet', trace = FALSE, preProc = c("center", "scale"))
model_nnet

# Gerando Matriz de Confusão
dfPred_nnet <- predict(model_nnet, newdata=marketing_test)
confusionMatrix(data=dfPred_nnet, marketing_test$SUBSCRIBED)

# Fazendo Scoring do Modelo
dfProbs_nnet <- predict(model_nnet, newdata=marketing_test, type="prob")
head(dfProbs_nnet)

# Armazenando os indicadores
pred_nnet <- predict(model_nnet, marketing_test)
RMSE_nnet <- round(rmse_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_nnet)), 2)
MAPE_nnet <- round(mape_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_nnet)), 2)
R2_nnet <- round(rsq_vec(as.numeric(marketing_test$SUBSCRIBED), as.numeric(pred_nnet)), 2)
ACCURACY_nnet <- round(accuracy_vec(marketing_test$SUBSCRIBED, pred_nnet),2)

##########################################
##########################################

# Resumo da Performance dos Modelos Testados

perf_mod <- tibble(
  Modelos = c("Regressão Logistica", "Arvore Simples", "Arvore Boosting", 
              "SVM Linear", "Redes Neurais"),
  RMSE = round(c(
    RMSE_glm, RMSE_arvoresimples, RMSE_arvoreboosting, RMSE_svmlinear, RMSE_nnet
  ), 3),
  MAPE = round(c(
    MAPE_glm, MAPE_arvoresimples, MAPE_arvoreboosting, MAPE_svmlinear, MAPE_nnet
  ), 3),
  R2 = (c(
    R2_glm, R2_arvoresimples, R2_arvoreboosting, R2_svmlinear, R2_nnet
  )*100) %>% 
    paste0("%"),
  ACCURACY = round(c(
    ACCURACY_glm, ACCURACY_arvoresimples, ACCURACY_arvoreboosting, ACCURACY_svmlinear, ACCURACY_nnet
  ), 5)
) 

resultado <- perf_mod %>% 
  kable(caption = "Performance dos Modelos") %>%
  kable_styling("striped", full_width = F)
resultado



