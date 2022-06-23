dados=read.csv2(file.choose(), head=T)

df=dados
head(df)
names(dados)
nrow(dados)
ncol(dados)

str(df1)
df$Income <- as.numeric(df$Income)

cor(df)

### Melhor Modelo
mod <- lm(sales ~.-1, data=df)
mod_step <- step(mod, direction="both")
summary(mod_step)

mod <- lm(sales ~., data=df)
summary(mod)

mod <- lm(sales ~.-1, data=df)
summary(mod)

mod <- lm(log(sales) ~ log(.), data=df)  
summary(mod)

mod <- lm(log(sales) ~ ., data=df)  
summary(mod)

mod <- lm(sales ~ sqrt(.), data=df)
summary(mod)

mod <-lm(sqrt(sales) ~ sqrt(.)-1, data=df) 
summary(mod)

mod <-lm(sales + tv + radio, data=df) 
mod_step <- step(mod, direction="both")
summary(mod_step)

mod=glm(sales ~ ., family = Gamma, data=df)
summary(mod)

mod <- glm(sales ~ ., family = binomial(link = "logit"), 
    data = df)
summary(mod)

mod=glm(sqrt(sales) ~ ., family = inverse.gaussian, data=df)
summary(mod)

mod <- glm.nb(sales^0.5 ~ ., data=df)
summary(mod)

mod <- glm(Balance~., family = poisson(link = "log"), data = df)
summary(mod)

### MELHOR MODELO CARLOS
mod <- glm(sales^2 ~ TV + radio, family = gaussian, data=df)

### Análise pressupostos   
anares <- rstandard(mod)
par(mfrow=c(2,2))

qqnorm(anares, ylab="Residuos", xlab="Quantis", main="QQPlot", col="royalblue")
qqline(anares, col="orange")

library(nortest)
ad.test(anares)

fit=fitted.values(mod)   ### Valores ajustados da variável resposta pelo modelo gerado
plot(fit, anares, ylab="Residuos", xlab="Valores Ajustados", main="Homocedasticidade")

library(lmtest)
bptest(mod)

plot(anares)
abline(0,0)

dwtest(mod)

library(MASS)
boxcox(mod, lambda = seq(0.1,0.8, 1/10), plotit = TRUE, data=df)
bc=boxcox(mod)

### Previsoes e intervalos de confianca
predict(mod, df, interval="predict")
predict(mod, df, interval="confidence")






### CARLOS QUINTANILHA
advertising_glmMod <- glm(sales^2 ~ TV + radio, family = gaussian, data=df)

### Análise pressupostos   
anares <- rstandard(advertising_glmMod)
par(mfrow=c(2,2))

qqnorm(anares, ylab="Residuos", xlab="Quantis", main="QQPlot", col="royalblue")
qqline(anares, col="orange")

library(nortest)
ad.test(anares)


fit=fitted.values(advertising_glmMod)   ### Valores ajustados da variável resposta pelo modelo gerado
plot(fit, anares, ylab="Residuos", xlab="Valores Ajustados", main="Homocedasticidade")

library(lmtest)
bptest(advertising_glmMod)

plot(anares)
abline(0,0)

dwtest(advertising_glmMod)

library(MASS)
boxcox(mod, lambda = seq(0.1,0.8, 1/10), plotit = TRUE, data=df)
bc=boxcox(advertising_glmMod)

### Previsoes e intervalos de confianca
predict(mod, df, interval="predict")
predict(mod, df, interval="confidence")

