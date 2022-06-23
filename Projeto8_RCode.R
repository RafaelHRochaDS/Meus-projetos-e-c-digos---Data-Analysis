########################################################################
# Desafios e Requisitos de Projetos Analiticos 
# Projeto Final - T8
# Rafael Rocha - A56660250
# Julho/2021
########################################################################

########################################################################
# Projeto: Waysides - Predicao de desgaste de rodeiros de vagoes GDT/GDU
# Cliente: Equipe de manutencao de rodas
########################################################################

# Carregando os pacotes
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(DataExplorer)
library(rsample)
library(magrittr) 
library(visdat)
library(imbalance)
library(themis)
library(tidymodels)   
library(baguette)     
library(workflowsets) 
library(patchwork)
library(skimr)

options(scipen=7)
options(warn=-1)
doParallel::registerDoParallel()
theme_set(theme_bw())

###########################################
######### Importando o dataset ############
###########################################

df <- read.csv(file="/Users/rafaeldesouza/EFC_WAYSIDES.csv", header=TRUE)

################################################
#### Analise exploratoria dos dados brutos #####
################################################
summary(df)
head(df)


df %>% 
  sample_frac(0.01) %>% 
  visdat::vis_dat()

###########################################
##### Pre-processamento dos dados #########
###########################################

df$VELOCIDADE_ENTRADA_TREM <- as.numeric(df$VELOCIDADE_ENTRADA_TREM)
df$VELOCIDADE_SAIDA_TREM <- as.numeric(df$VELOCIDADE_SAIDA_TREM)
df$ANGULO_FRISO_RODA <- as.numeric(df$ANGULO_FRISO_RODA)
df$ALTURA_FRISO_RODA <- as.numeric(df$ALTURA_FRISO_RODA)
df$ESPESSURA_FRISO_RODA <- as.numeric(df$ESPESSURA_FRISO_RODA)
df$CAVA_RODA <- as.numeric(df$CAVA_RODA)
df$ANGULO_ATAQUE_EIXO_FRONTAL_TRUQUE <- as.numeric(df$ANGULO_ATAQUE_EIXO_FRONTAL_TRUQUE)
df$ANGULO_ATAQUE_EIXO_TRASEIRO_TRUQUE <- as.numeric(df$ANGULO_ATAQUE_EIXO_TRASEIRO_TRUQUE)
df$TRACKING_POSITION_EIXO_FRONTAL_TRUQUE <- as.numeric(df$TRACKING_POSITION_EIXO_FRONTAL_TRUQUE)
df$TRACKING_POSITION_EIXO_TRASEIRO_TRUQUE <- as.numeric(df$TRACKING_POSITION_EIXO_TRASEIRO_TRUQUE)
df$ROTACAO_EIXO <- as.numeric(df$ROTACAO_EIXO)
df$ALINHAMENTO_ENTRE_EIXOS_.IAM. <- as.numeric(df$ALINHAMENTO_ENTRE_EIXOS_.IAM.)
df$DESLOCAMENTO_ENTRE_EIXOS_.SHIFT. <- as.numeric(df$DESLOCAMENTO_ENTRE_EIXOS_.SHIFT.)
df$TRACKING_ERROR_.TE. <- as.numeric(df$TRACKING_ERROR_.TE.)
df$SERPENTEAMENTO_.HUNTING. <- as.numeric(df$SERPENTEAMENTO_.HUNTING.)

str(df)

######################################################################################
# Transformando a variavel target de espessura do friso 
df <- mutate(df, ESPESSURA_FRISO_RODA_2 = case_when(ESPESSURA_FRISO_RODA<26 ~1, ESPESSURA_FRISO_RODA>=26 ~0))
df$ESPESSURA_FRISO_RODA_2 <- as.factor(df$ESPESSURA_FRISO_RODA_2)

######################################################################################
# Uma visão geral das classes das features numéricas em relação a variavel target
num_columns <- c(df %>% select_if(is.numeric) %>% colnames(), 'ESPESSURA_FRISO_RODA_2')
df%>% 
  select_at(num_columns) %>% 
  select(-ROW_ID) %>%
  gather(key, value, -ESPESSURA_FRISO_RODA_2) %>%
  ggplot(aes(y=ESPESSURA_FRISO_RODA_2, x=value))+
  geom_boxplot()+
  facet_wrap(~key, ncol=5, scales = "free_x")+
  labs(x = "", y="")
# Parece que algumas features possuem comportamentos diferentes quando avaliados segundo a target. 
# Além disso é possível notar que a maioria delas possue outliers que serao removidos mais a frente

######################################################################################

plot_intro(df, ggtheme = theme_bw(), 
           theme_config = list(legend.position = "bottom"))

# Notamos que 1.4% dos dados é faltante, por ser um percentual baixo vamos retirá-los da base
# Vamos olhar a estrutura dessa base de maneira mais aprofundada e confirmar a presença de NA'S

df %>% 
  sample_frac(0.01) %>% 
  visdat::vis_dat()


######################################################################################
# Removendo NULL's e NA's que aparentam ter entrado na base como erro de medicao
df[ df == "NULL"] <- NA 
df <- na.omit(df)

######################################################################################
# Dropping colunas irrelevantes da tabela para a modelagem
df[ ,c(1,2,3,4,5,6,7,8,9,10,13,22,26)] <- list(NULL)
View(df)

######################################################################################
# Detectando visualmente a presenca de outliers
names <- c("VET", "VST", "ANGFR", "ALTFR", "EFR", "CR", "AAEFT", 
           "AAETT", "TPEFT", "TPETT", "AEE", "DEE", "TE", "EFR_2")
boxplot(df, names=names, col = "bisque", main="Detectando outliers \n *Variáveis abreviadas pelas iniciais")$out

# Definindo funcao para identificar e remover outliers

is_outlier <- function(x, na.rm = FALSE) {
  qs = quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  
  lowerq <- qs[1]
  upperq <- qs[2]
  iqr = upperq - lowerq 
  
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  
  x > extreme.threshold.upper | x < extreme.threshold.lower
}

removing_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    cat("Removing outliers in column: ", col, " \n")
    df <- df[!is_outlier(df[[col]]),]
  }
  df
}

# Definindo as features de interesse para remocao dos outliers

var_de_interesse <- c('VELOCIDADE_ENTRADA_TREM', 'VELOCIDADE_SAIDA_TREM', 'ANGULO_FRISO_RODA', 'ALTURA_FRISO_RODA',
                      'ESPESSURA_FRISO_RODA', 'CAVA_RODA', 'ANGULO_ATAQUE_EIXO_FRONTAL_TRUQUE',
                      'ANGULO_ATAQUE_EIXO_TRASEIRO_TRUQUE', 'TRACKING_POSITION_EIXO_FRONTAL_TRUQUE',
                      'TRACKING_POSITION_EIXO_TRASEIRO_TRUQUE', 'ALINHAMENTO_ENTRE_EIXOS_.IAM.', 
                      'DESLOCAMENTO_ENTRE_EIXOS_.SHIFT.', 'TRACKING_ERROR_.TE.')

df <- removing_outliers(df, var_de_interesse)

# Foram removidos da base 25.521 outliers

######################################################################################
# Uma última visualizacao com diferentes insights das variaveis numericas da nossa base já pre processada
skim(df)

######################################################################################
# Analisando desbalanceamento na base com biblioteca imbalance

imbalanceRatio(df, classAttr = "ESPESSURA_FRISO_RODA_2")
# O dataset apresenta apenas 1% de desequilibrio ou desbalanceamento. 
# Aplicar tecnicas de oversampling a partir de 1% da base para ficar equilibrada
# nao parece uma boa ideia. Vamos aplicar no lugar tecnicas de undersampling para balancear a base.

######################################################################################
# Salvando o dataset pre processado para ser importado pro SPARK
# write.csv(df,"/Users/rafaeldesouza/EFC__WAYSIDES.csv", row.names = FALSE)

###########################################
######### Analise exploratoria ############
###########################################

introduce(df)

#############################################
##### Analise visual da variavel target #####
#############################################

table(df$ESPESSURA_FRISO_RODA_2) 
# Distribuicao da variavel target. Constam na base 2097905 obs (>=26=0) 
# e 22442 obs (<26=1) que representam friso baixo. 

plot1 <- df %>% 
  count(ESPESSURA_FRISO_RODA_2) %>% 
  ggplot(aes(x=rev(ESPESSURA_FRISO_RODA_2), y=n, fill=ESPESSURA_FRISO_RODA_2))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position = "bottom")+
  labs(y="Número de instâncias", x = "")

plot2 <- df %>% 
  count(ESPESSURA_FRISO_RODA_2) %>% 
  arrange(desc(ESPESSURA_FRISO_RODA_2)) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )%>% 
  ggplot(aes(x="", y=prop, fill=ESPESSURA_FRISO_RODA_2)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos,
                label = paste(scales::comma(n, big.mark = "."),
                              scales::comma(n/sum(n), big.mark = ".", 
                                            suffix = "%" ),sep = "\n")
  ), 
  color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

plot1 + plot2

#########################################
############# Correlacao ################
#########################################

df_plot_corr <- df
names(df_plot_corr) <- c('VEL_ENT_TREM', 'VEL_SAIDA_TREM', 'ANG_FRISO_RODA', 'ALT_FRISO_RODA',
                         'ESP_FRISO_RODA', 'CAVA_RODA', 'ANG_AT_EIXO_F_TQ',
                         'ANG_AT_EIXO_T_TQ', 'T_P_EIXO_F_TQ',
                         'T_PO_EIXO_T_TQ', 'AL_EIXOS_.IAM.', 
                         'DESL_EIXOS_.SHIFT.', 'T_ERROR_.TE.')

plot_correlation(df_plot_corr)

#########################################
#########################################
#########################################
















##########################################################################################
######### NAO ######## ENTRARAM ######## NO ######## MARKDOWN ############################
##########################################################################################

# Boxplot velocidade de entrada dos trens
plot1.1 <- ggplot(df, aes(x=VELOCIDADE_ENTRADA_TREM)) +  
  geom_boxplot(fill="coral1", alpha=0.8) +
  labs(title = "Distribuição da velocidade de ENTRADA dos trens no Waysides \nEm Km/h") +
  theme_minimal()+
  theme(plot.title=element_text(family='', face='bold', colour='black', size=12))+
  xlab("Km/h")
print(plot1.1)

# Boxplot velocidade de saida dos trens
plot1.2 <- ggplot(df, aes(x=VELOCIDADE_SAIDA_TREM)) +  
  geom_boxplot(fill="coral1", alpha=0.8) +
  labs(title = "Distribuição da velocidade de SAÍDA dos trens no Waysides \nEm Km/h") +
  theme_minimal()+
  theme(plot.title=element_text(family='', face='bold', colour='black', size=12))+
  xlab("Km/h")
print(plot1.2)

##########################################################################################
# Densidade angulos de ataque dos eixos frontal e traseiro
plot2.1 <- ggplot(df, aes(x=ANGULO_ATAQUE_EIXO_FRONTAL_TRUQUE)) +  
  geom_dotplot(fill="red", alpha=0.4, binwidth = 3/28) +
  labs(title = "Medições dos sensores dos eixos de ataque \nTRASEIRO - Em mrad") +
  theme_minimal()+
  theme(plot.title=element_text(family='', face='bold', colour='black', size=12))+
  xlab("mrad")+
  ylab("Densidade")
print(plot2.1)

plot2.2 <- ggplot(df, aes(x=ANGULO_ATAQUE_EIXO_TRASEIRO_TRUQUE)) +  
  geom_dotplot(fill="red", alpha=0.4, binwidth = 3/28) +
  labs(title = "Medições dos sensores dos eixos de ataque \nTRASEIRO - Em mrad") +
  theme_minimal()+
  theme(plot.title=element_text(family='', face='bold', colour='black', size=12))+
  xlab("mrad")+
  ylab("Densidade")
print(plot2.2)

##########################################################################################
# Boxplot cava roda
plot3 <- ggplot(df, aes(x=CAVA_RODA)) +  
  geom_boxplot(fill="cadetblue", alpha=0.8) +
  labs(title = "Distribuição das medições - Cava da roda \nEm mm") +
  theme_minimal()+
  theme(plot.title=element_text(family='', face='bold', colour='black', size=12))+
  xlab("mm")
print(plot3)

##########################################################################################
# Plotando alinhamento e deslocamento entre eixos
ggplot(df, aes(x = ALINHAMENTO_ENTRE_EIXOS_.IAM.)) + geom_density()
ggplot(df, aes(x = DESLOCAMENTO_ENTRE_EIXOS_.SHIFT.)) + geom_density()



