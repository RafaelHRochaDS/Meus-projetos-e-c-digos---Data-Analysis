dados=read.csv2(file.choose(), head=T)
df=dados
head(df)
names(dados)
nrow(dados)
ncol(dados)

install.packages("dplyr")
install.packages("magrittr")
library(dplyr)

College <- read.csv2(file.choose(), head=T) %>% 
  rename(.rownames = X1)




mod <- lm(Apps ~., data=df)
summary(mod)

mod <- lm(Volume ~ Girth, data=df)
summary(mod)

mod <- lm(Volume ~ Height, data=df)
summary(mod)

mod <- lm(Volume ~ sqrt(Girth) + sqrt(Height), data=df)
summary(mod)

### Melhor Modelo ??
mod <-lm(Volume ~. -1, data=df)
summary(mod)

###
mod=glm(log(Volume) ~ ., family = Gamma, data=df)
summary(mod)

mod=glm(Volume ~., family = Gamma, data=df)
summary(mod)

mod=glm(sqrt(Volume) ~ ., family = inverse.gaussian, data=df)
summary(mod)

mod=lm(Volume ~ log(.), data=df)
summary(mod)

mod <- glm.nb(Volume^0.5 ~ ., data=df)
summary(mod)

### Análise pressupostos   
anares <- rstandard(mod)
par(mfrow=c(2,2))

qqnorm(anares, ylab="Residuos", xlab="Quantis", main="QQPlot", col="royalblue")
qqline(anares, col="orange")

library(nortest)
ad.test(anares)
shapiro.test(anares)


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
predict(mod, interval="predict")
predict(mod, interval="confidence")