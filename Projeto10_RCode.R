dados=read.csv2(file.choose(), head=T)
df=dados
head(df)
names(dados)
nrow(dados)
ncol(dados)

str(df)
df$Income <- as.numeric(df$Income)
df$Limit <- as.numeric(df$Limit)
df$Rating <- as.numeric(df$Rating)
df$Cards <- as.numeric(df$Cards)
df$Age <- as.numeric(df$Age)
df$Education <- as.numeric(df$Education)
df$Gender <- as.numeric(df$Gender)
df$Student <- as.numeric(df$Student)
df$Married <- as.numeric(df$Married)
df$Ethnicity <- as.numeric(df$Ethnicity)
df$Balance <- as.numeric(df$Income)


cor(df)

mod <- lm(Balance ~., data=df)
summary(mod)

mod <- lm(Balance ~.-1, data=df)
summary(mod)

mod <- lm(Balance ~ Rating-1, data=df)
summary(mod)

mod <- lm(log(Balance) ~ log(Rating)+Income, data=df) ### Melhor Modelo 
mod_step <- step(mod, direction="both")
summary(mod_step)

mod <- lm(Balance ~ sqrt(Rating), data=df)
summary(mod)

mod <-lm(log(Balance) ~.-1, data=df) #
summary(mod)

mod <-lm(log(Balance) ~.-1, data=df) ### 
mod_step <- step(mod, direction="both")
summary(mod_step)

mod <-lm(Balance ~ Income + Limit + Cards + Age + Student, data=df)
summary(mod)   
   

###
mod=glm(Balance ~ Rating + Income, family = Gamma, data=df)
summary(mod)

mod=glm(sqrt(Balance) ~ Rating + Income, family = inverse.gaussian, data=df)
summary(mod)

mod=lm(Balance ~ log(.), data=df)
summary(mod)

mod <- glm.nb(Balance^0.5 ~ ., data=df)
summary(mod)

mod <- glm(Balance~., family = poisson(link = "log"), data = df)
summary(mod)



### Análise pressupostos   
anares <- rstandard(mod_step)
par(mfrow=c(2,2))

qqnorm(anares, ylab="Residuos", xlab="Quantis", main="QQPlot", col="royalblue")
qqline(anares, col="orange")

library(nortest)
ad.test(anares)
shapiro.test(anares)


fit=fitted.values(mod_step)   ### Valores ajustados da variável resposta pelo modelo gerado
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


