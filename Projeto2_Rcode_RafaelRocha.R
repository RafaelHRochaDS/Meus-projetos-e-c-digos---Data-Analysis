###########################
###########################
##########################
# Rafael Rocha - A56660250 - FGV Turma 8
# Análise de Séries Temporais 
# Professor: Alvaro Villarinho

### Estudo de caso 
# Análise da Demanda de Fertilizantes no Brasil 
# Período mensal de janeiro de 1998 a abril de 2020
# Subconjunto de treino de Jan2007 a Dez2018 
# Subconjunto de teste de Jan2019 a Abr2020
# Fonte dos dados: http://anda.org.br/estatisticas/ 

# Carregando os pacotes   
library(dplyr)  
library(car) 
library(rstatix) 
library(emmeans)
library(ggplot2)
library(knitr)
library(kableExtra)
library(htmltools)
library(fpp2)
library(forecast)
library(tseries)
library(seasonal)
library(tidyverse)
library(Quandl)
library(xlsx)
library(readxl)
library(gridExtra)
library(TSstudio)
library(BETS)
library(rmarkdown)

# Análise exploratória dos dados  
## Carregando os dados 
demanda <- read_excel("~/Desktop/AnaliseDeSeriesTemporais/Demanda.xlsx")
glimpse(demanda)

## Verificando a classe
class(demanda)

## Verificando como começam e terminam os dados
head(demanda)
tail(demanda)

## Transformando os dados em TS e conferindo a janela e classe
ts.demanda <- ts(demanda, frequency = 12, start = c(1998, 1))
head(ts.demanda)
tail(ts.demanda)
class(ts.demanda)

## Plotando a TS
autoplot(ts.demanda, xlab = "ano", 
         ylab="Entrega de fertilizantes ao mercado",
         main = "Demanda mensal em mil toneladas janeiro de 1998 a abril de 2020", col ="blue", lwd=2)

## Plotando a TS e analisando a sazonalidade. 
ggseasonplot(ts.demanda, year.labels = TRUE, year.label.left = TRUE)+
  ylab("Entrega de fertilizantes ao mercado")+ xlab("Ano")+
  ggtitle("Demanda mensal em mil toneladas janeiro de 1998 a    abril de 2020")

## Análise da TS com boxplot
boxplot(ts.demanda~cycle(ts.demanda),col = rep(c("gold","darkgreen",
                                                 "blue", "red"), each=3),
        xlab="Mês", ylab= "Período de janeiro de 1998 a abril de 2020",
        main ="Demanda mensal de fertilizantes em mil toneladas",
        par(bg="white"))

## Análise exploratória dos padrões usando o pacote TSstudio
ts_seasonal(ts.demanda, type = "all")

## Decompondo as séries temporais
d1 <- ts.demanda%>%
  decompose(type= "additive")%>%
  autoplot() + xlab("Ano") + ylab("Demanda em mil toneladas")+
  ggtitle("Decomposição Aditiva")

d2 <- ts.demanda%>%
  decompose(type= "multiplicative")%>%
  autoplot() + xlab("Ano") + ylab("Demanda em mil toneladas")+
  ggtitle("Decomposição Multiplicativa")

grid.arrange(d1,d2,ncol = 2)

## Teste e treino
ts.train <- window(ts.demanda, start=c(2007,1),end=c(2018,12))
ts.test <- window(ts.demanda, start=c(2019,1),end=c(2020,04))

autoplot(ts.demanda)+
  autolayer(ts.train)+
  autolayer(ts.test)

## Decompondo os modelos
g1 <- ts.demanda%>%
  decompose(type= "additive")%>%
  autoplot() + xlab("Ano")+
  ggtitle("Decomposição Aditiva")

g2 <- ts.demanda%>%
  decompose(type= "multiplicative")%>%
  autoplot() + xlab("Ano")+
  ggtitle("Decomposição Multiplicativa")

grid.arrange(g1,g2,ncol = 2)

## Selecionando nossos modelos
#### Holt Winters Aditivo
fit_HW_adit <- hw(ts.train, seasonal="additive") 

previsao.fit_HW_adit<- forecast(fit_HW_adit, h=21)

autoplot(ts.train, series ="Dados treino") +
  autolayer(previsao.fit_HW_adit, series="HW aditivo") +
  autolayer(ts.test, series="Série de teste", showgap = FALSE) +
  xlab("Mês/Ano")+ylim(c(0,8000))

#### Holt Winters Multiplicativo
fit_HW_mult <- hw(ts.train, seasonal="multiplicative")

previsao.fit_HW_mult<- forecast(fit_HW_mult, h=21)

autoplot(ts.train, series ="Dados treino") +
  autolayer(previsao.fit_HW_mult, series = "HW Multiplicativo")+
  autolayer(ts.test, series="Série de teste", showgap = FALSE) +
  xlab("Mês/Ano")+ylim(c(0,8000))

#### Plotando os dois modelos
autoplot(ts.demanda) +
  autolayer(fit_HW_adit, series="HW aditivo", PI=FALSE) +
  xlab("Ano") + ylab("Demanda em mil toneladas")+
  autolayer(fit_HW_mult, series="HW multiplicativo",
            PI=FALSE) +
  xlab("Ano") + ylab("Demanda em mil toneladas") +
  ggtitle("Entrega de fertilizantes ao mercado em mil toneladas") +
  guides(colour=guide_legend(title="Forecast"))

#### Analisando as saídas dos modelos de Holt
summary(fit_HW_adit)
summary(fit_HW_mult)
#A RMSE, MAPE e MASE são menores no modelo aditivo, portanto ele seria o melhor modelo a princípio.

#### Modelo AUTOARIMA
autoarima <- auto.arima(ts.train, seasonal= TRUE,
                        stepwise = FALSE, 
                        approximation = FALSE)

previsao.autoarima<- forecast(autoarima, h=21)

autoplot(ts.train, series ="Dados treno") +
  autolayer(previsao.autoarima, series = "Modelo AUTO.ARIMA")+
  autolayer(ts.test, series = "Série de teste", showgap = FALSE) +
  xlab("Mês/Ano")+ylim(c(0,8000))

#### Testando acurácias - Base Treino x Modelo
accuracy(fit_HW_adit$fitted,ts.train)
accuracy(fit_HW_mult$fitted,ts.train)
accuracy(autoarima$fitted,ts.train)

#### Testando acurácias - Base Teste x Previsão do Modelo
accuracy(previsao.fit_HW_adit$mean,ts.test)
accuracy(previsao.fit_HW_mult$mean,ts.test)
accuracy(previsao.autoarima$mean,ts.test)
#O modelo autoarima parece estar se ajustando bem à série.

## ACF e PACF
ggtsdisplay(ts.demanda)
ggAcf(ts.demanda, lag.max = 60)
ggPacf(ts.demanda, lag.max = 60)

diff <- diff(ts.demanda)
ggtsdisplay(diff)
acf.diff <- ggAcf(diff)
pacf.diff <- ggPacf(diff)
grid.arrange(acf.diff, pacf.diff, nrow =2)

#### diff
diff <- diff(ts.demanda)
ggtsdisplay(diff)
acf.diff <- ggAcf(diff)
pacf.diff <- ggPacf(diff)
grid.arrange(acf.diff, pacf.diff, nrow =2)

#### diff_BoxCox
lambda <- BoxCox.lambda(ts.demanda)
ts.boxcox <- BoxCox(ts.demanda,lambda = lambda)
ggtsdisplay(ts.boxcox)
diff_boxcox <- diff(ts.boxcox)
ggtsdisplay(diff_boxcox)

#### diff lag6
diff_6 <- diff(ts.demanda,lag = 6)
ggtsdisplay(diff_6)
acf.diff <- ggAcf(diff)
pacf.diff <- ggPacf(diff)
grid.arrange(acf.diff, pacf.diff, nrow =2)

#### diff lag12
diff_12 <- diff(ts.demanda,lag = 12)
ggtsdisplay(diff_12)
acf.diff <- ggAcf(diff)
pacf.diff <- ggPacf(diff)
grid.arrange(acf.diff, pacf.diff, nrow =2)

#Se nota claramente que os valores demoram a cair para zero, o que reforça a tese de não  estacionaridade da série. 
#O uso de lag 12 apresenta melhores resultados que o lag 6. 
#Fica aparente um componente sazonal significativo.  

## Testes de raíz-unitária
### **Regra de ouro -> P-valor baixo: rejeita Ho.**

# Teste ADF                     #
#################################
# Ho: A série não é estacionária 
# H1: A série é estacionária
adf.test(ts.train)
#Com p-value de 1% podemos rejeitar a Ho de não estacionaridade.

# Teste KPSS               #
############################
# Ho: A série é estacionária    
# H1: A série não é estacionária
kpss.test(ts.train)
#Com o resultado do teste KPSS, com 1% de p-value, rejeitamos Ho, indica que a série é não estacionária.

# Teste Philipps-Perron      #
################################
# Ho: A série não é estacionária
# H1: A série é estacionária
pp.test(ts.train)
#Com p-value de 1% podemos rejeitar a Ho de não estacionaridade.
#Temos um placar de 2x1.

# Usando funções ndiffs e nsdiffs
ndiffs(ts.train)
nsdiffs(ts.train)
#Esses testes sugerem um número de diferenciações e diferenciações sazonais para tornar nossa série estacionária. 
#Aplicaremos nos valores de d e D dos modelos sarima e arima.

## Construindo o modelo
# Utilização de AR(12) sugerido na figura PACF.
#######Modelo AR
ar <- Arima(ts.train, order = c(12,1,0))

#Efetuando as previsões
previsao.ar <- forecast(ar, h = 21) # Com IC level = 95
autoplot(ts.train, series = "Treino") +
  autolayer(previsao.ar$fitted, series = "Modelo AR")+
  autolayer(previsao.ar, series = "Predição")+
  autolayer(ts.test, series = "Teste")

#######Modelo Ma
ma <- Arima(ts.train, order = c(0,1,3))
ma

#Efetuando as previsões
previsao.ma <- forecast(ma, h = 21) # Com IC level = 95
autoplot(ts.train, series = "Treino") +
  autolayer(previsao.ma$fitted, series = "Modelo MA")+
  autolayer(previsao.ma, series = "Previsão")+
  autolayer(ts.test, series = "Teste")

######Modelo Arima
# Modelo Arima
arima <- Arima(ts.train, order = c(3,1,3))
arima

#Efetuando as previsões
previsao.arima <- forecast(arima, h = 21) # Com IC level = 95
autoplot(ts.train, series = "Treino") +
  autolayer(previsao.arima$fitted, series = "Modelo ARIMA")+
  autolayer(previsao.arima, series = "Previsão")+
  autolayer(ts.test, series = "Teste")

######Modelo Sarima
sarima <- Arima(ts.train, order = c(1,1,2),
                seasonal = list(order= c(2,1,2), 
                                period =12))
sarima

#Efetuando as previsões
previsao.sarima <- forecast(sarima, h =21) # Com IC level = 95
autoplot(ts.train, series = "Treino") +
  autolayer(previsao.sarima$fitted, series = "Modelo SARIMA")+
  autolayer(previsao.sarima, series = "Previsão")+
  autolayer(ts.test, series = "Teste")

## Testando os modelos em acurácia e AIC
t_test(model = ar)
t_test(model = ma)
t_test(model = arima)
t_test(model = sarima)
t_test(model = autoarima)
AIC(ar)
AIC(ma)
AIC(arima)
AIC(sarima)
AIC(autoarima)

##Testes de diagnósticos
#Efetuando os testes de diagnósticos para o modelo SARIMA(p,d,q)(P,D,Q)[s] 
#(Ausência de autocorrelação serial; ausência de heterocedasticidade condicional; normalidade).
##Modelo HW_ADITIVO
resfit_HW_adit <- checkresiduals(fit_HW_adit, col ="red") 

##Modelo HW_MULTIPLICATIVO
resfit_HW_mult <- checkresiduals(fit_HW_mult, col ="red")

##Modelo AR
resar <- checkresiduals(ar, col ="red")

##Modelo MA
resma <- checkresiduals(ma, col ="red")

##Modelo ARIMA
resarima <- checkresiduals(arima, col ="red")

#Modelo SARIMA 
ressarima <- checkresiduals(sarima, col ="red")

#Modelo AUTOARIMA
resautoarima <- checkresiduals(autoarima, col ="red")

#Testando o modelo vs resultado 
#Modelo HWAditivo
resfit_HW_adit$p.value
#Modelo HWMultiplicativo	
resfit_HW_mult$p.value
#Modelo AR	 
resar$p.value
#Modelo MA
resma$p.value
#Modelo ARIMA
resarima$p.value
#Modelo AUTOARIMA
resautoarima$p.value

## Definição do melhor modelo
#Usando o comando `autoarima` os melhores modelos encontrados foram ARIMA(1,1,2)(2,1,2)[12] e ARIMA(1,0,0)(2,1,0)[12] 
#Os dois modelos apresentaram ausência de autocorrelação serial, ausência de heterocedasticidade condicional e normalidade. 
#Por AIC, o primeiro modelo apresenta melhor resultado que o segundo.

## Previsão usando o nosso melhor modelo
# Faremos a previsão com base no subconjunto de treinamento (train) para o período até dezembro-2021. 
# Faremos comparação com a base de teste.
previsao.sarima <- forecast(sarima,h = 36)
autoplot(ts.train, series = "Base de Treino") +
  autolayer(previsao.sarima$fitted, series = "Modelo SARIMA")+
  autolayer(previsao.sarima, series = "Previsão")+
  autolayer(ts.test, series = "Base de Teste")

kable(previsao.sarima, format = "html",row.names = T) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"))

## Conclusão 
#Baseados nos dados reais do setor, ilustrados na figura acima, 
#especificamente dos meses de maio, junho e julho de 2020, podemos confirmar que nossas previsões acertaram, 
#o que valida o modelo sugerido nesse projeto. 

