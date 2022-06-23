### QUAKES

library(magrittr)

dados_quakes <- (quakes)
df <- dados_quakes
head(df)

cor(df)

mod = lm(mag ~ lat + long + depth + stations, data = df)
summary(mod)
## de outra forma
mod2=lm(mag ~., data=df)
summary(mod2)
## modelo 3
mod3=step(mod2, direction = "backward")
summary(mod3)
## modelo 4
mod4=lm(mag ~ lat + long, data = df)
summary(mod4)
## modelo 5
mod5=lm(mag ~ depth + stations, data = df)
summary(mod5)
## modelo 6
mod6=step(mod2, direction = "both")
summary(mod6)
 
### Propondo transformações nas variáveis
## modelo 7
mod7_log <- lm(log(mag) ~ log(.),data = df)
mod_mag_step <- step(mod7_log, direction = "both")
summary(mod_mag_step)

## modelo 8 - otimizacao do modelo 2
mod2<-lm(mag ~., data=df)
mod_mag_step <- step(mod2, direction = "both")
summary(mod_mag_step)



## Análise de Resíduos com modelo 2 que teve melhor R2.
anares <- rstandard(mod_mag_step)
par(mfrow=c(2,2))

### Teste de Normalidade - Gráfico
qqnorm(anares, ylab="Residuos", xlab="Quantis", main="QQPlot", col="royalblue")
qqline(anares, col="orange")

### Teste formais de Normalidade
library(nortest)
ad.test(anares)


### Teste de Homocedasticidade - Gráfico
fit=fitted.values(mod2)   ### Valores ajustados da variável resposta pelo modelo gerado
plot(fit, anares, ylab="Residuos", xlab="Valores Ajustados", main="Homocedasticidade")
abline(0,0)

### Teste formal de Homocedasticidade
library(lmtest)
bptest(mod2)

### Teste de Autocorrelação - Gráfico
plot(anares)
abline(0,0)

### Teste formal de Autocorrelação
dwtest(mod2)

### Previsoes e intervalos de confianca
predict(mod2, interval="predict")
predict(mod2, interval="confidence")


### Rosi
set.seed(35) 
df2    <- df1 %>% slice(sample(1:nrow(df))) 
valida  <- df2 %>% slice(1:80) 
treino <- df2 %>% slice(81:nrow(.))

previs <- predict(mod1, valid, interval = "predict") %>% 
  round(2)



