library(tidyverse)
library(readxl)
library(GGally)
library(caret)
library(ggcorrplot)
library(MASS)
library(ISLR)
library(glmnet)
library(lmtest)
library(nortest)
library(kableExtra) # kbl table
library(ggtext)# biblioteca para a funcao geom_richtext
library(gridExtra)
library(grid)

base = read.csv2("Dataset_Cellphone.csv", sep=",")
names(base)
head(base)

base$Churn = as.factor(base$Churn)
base$ContractRenewal = as.factor(base$ContractRenewal)
base$DataPlan = as.factor(base$DataPlan)

base$DataUsage = as.numeric(base$DataUsage)
base$DayMins = as.numeric(base$DayMins)
base$MonthlyCharge = as.numeric(base$MonthlyCharge)
base$OverageFee = as.numeric(base$OverageFee)
base$RoamMins = as.numeric(base$RoamMins)

summary(base)

churn.1 = base %>%
  filter(Churn == "1")

churn.0 = base %>%
  filter(Churn == "0")

#churn.0 = which(base$Churn == 0)

table(base$Churn)

base = rbind(churn.1, churn.0[sample(nrow(churn.1), length(churn.1$Churn)), ])

table(base$Churn)

#base = base%>%
  
# Modelo
fit.1 <- glm( Churn ~ ., family = binomial, data = base)
summary(fit.1)

# Modelo 2
fit.2 <- glm( Churn ~ ., family = "binomial", data = base)
summary(fit.2)

# Modelo 3
fit.3 <- glm( Churn ~ ContractRenewal + CustServCalls + DataPlan+ RoamMins, family = binomial(link="logit"), data = base)
summary(fit.3)


anova(fit.3, fit.1, test="Chisq")

# Y ajustado
prop_ajustada <- fit.2$fitted.values
base$y_ajustado <- prop_ajustada

plot(base$y_ajustado,base$Churn,pch=16,xlab="y ajustado", ylab="y verdadeiro")
abline(coef = c(0,1),col=2,lwd=2)


# analise dos residuos
res <- rstandard(fit.2, type = "pearson") 

D.1 <- tibble(y.hat = base$y_ajustado, cov = base$churn, r = res)

g.1 = ggplot(D.1, aes(x = y.hat, y = r)) +
  geom_point(size=2) +
  labs(x='semana',y='resíduos')+
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank()) + 
  geom_hline(aes(yintercept = 0), col="gray60") + 
  geom_hline(aes(yintercept = -2), col="gray20",lty=2) + 
  geom_hline(aes(yintercept = 2), col="gray20",lty=2)


g.2 = ggplot(D.1,aes(sample=r)) + 
  stat_qq() +
  stat_qq_line(col="gray60", lwd=1.1) +
  labs(x='quantis teóricos',y='quantis amostrais') +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank())

grid.arrange(g.1,g.2,ncol=2)



# Modelo stepwise
step.model <- stepAIC(fit.1,direction = "both", trace = FALSE)
summary(step.model)



# Modelo lasso

X <- model.matrix(Churn~ ., base)[ ,-1]
Y <- base$Churn

cv.fit = cv.glmnet(X,Y,alpha = 1, family = "binomial")
plot(cv.fit)

(lambda = cv.fit$lambda.min)

lasso.model <- glmnet(X, Y, alpha = 1, family = "binomial",lambda = cv.fit$lambda.min)

# estimando o modelo nulo para calcular o pseudo R²
fit.nulo <- glm(Churn~1, family=binomial(link="logit"),  data = base)
summary(fit.nulo)

Modelo <- c("Logit","Fit.3","step", "lasso")
deviance <- c(deviance(fit.1), deviance(fit.3),deviance(step.model),deviance(lasso.model))
AIC <- c(fit.1$aic,fit.3$aic, step.model$aic, 0)
pR2 <- round(c(1-(logLik(fit.1)/logLik(fit.nulo)), 1-(logLik(fit.3)/logLik(fit.nulo)), 1-(logLik(step.model)/logLik(fit.nulo)), 0),3)

R <- data.frame(Modelo,AIC,Deviance=deviance,"pseudo R²"= pR2)
R














## 1. Métodos de regularização (LASSO)
## 2. Regressao logistica Geral


library(mlbench)
library(tidyverse)
library(caret)
library(glmnet)
library(sAIC)
library(readxl)


## Objetivo: prever a probabilidade de ser diabético a partir de diversas 
## variáveis clínicas.

#data("PimaIndiansChurn2", package = "mlbench")
#base <- na.omit(PimaIndiansChurn2)

# recorte dos base
head(base, 3)
dim(base)

#pregnant - Número de vezes que engravidou
#glucose - Concentração de glicose no plasma (teste de tolerância à glicose)
#pressure - pressão arterial diastólica (mm Hg)
#triceps - Espessura da dobra cutânea do tríceps (mm)
#insulin - insulina sérica de 2 horas (mu U / ml)
#mass - Índice de massa corporal (peso em kg / (altura em m) \ ^ 2)
#pedigree - Churn Pedigree Function (valores que medem a tendência 
#           ao desenvolvimento de Churn com base nas relações genéticas 
#           do indivíduo)
#age - Idade (anos)
#Churn - Variável de classe de Churn (teste para Churn)



## Dividindo aleatoriamente os base em conjunto de treinamento (80% para 
## construir o modelo) e conjunto de teste (20% para avaliar a capacidade 
## preditiva do modelo).

set.seed(100)
amostras.treino <- base$Churn %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data <- base[amostras.treino, ]
test.data <- base[-amostras.treino, ]


# Organizando os base (essa função já coloca variáveis categóricas como dummies)
x <- model.matrix(Churn~., train.data)[,-1]

# Transformando as categorias de y em classes numéricas
y <- ifelse(train.data$Churn == 1, 1, 0)

table(y)
# Encontrando o melhor lambda via cross-validation

set.seed(100) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)

cv.lasso$lambda.min          # lambda que minimiza a deviance
#log(cv.lasso$lambda.min)  

# A função cv.glmnet() também encontra o valor de lambda que fornece um modelo 
# mais simples, mas que está dentro de um erro padrão do valor ideal de lambda.

cv.lasso$lambda.1se
#log(cv.lasso$lambda.1se)


coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)


## Comparando a acurácia dos modelos

cv.fit = cv.glmnet(x,y,alpha = 1, family = "binomial")
plot(cv.fit)


## Modelo com lambda.min
modelo.1 <- glmnet(x,y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
coef(modelo.1)

#(lambda = cv.fit$lambda.min)

AIC.1 <- sAIC(x=x, y=y, beta=coef(modelo.1), family="binomial")
AIC.1
BIC.1= AIC.1$AIC[1]-2*length(coef(modelo.1))+(length(coef(modelo.1))*log(length(train.data$Churn)))

#modelo <- glmnet(x,y, alpha = 1, family = "binomial")
#plot(modelo, xvar = "lambda", label = TRUE)
#abline(v=log(cv.lasso$lambda.min))
#abline(v=log(cv.lasso$lambda.1se))


# Fazendo a previsão para o dado de teste
x.test <- model.matrix(Churn ~., test.data)[,-1]
prob1 <- exp(modelo.1 %>% predict(newx = x.test))/(1+exp(modelo.1 %>% predict(newx = x.test)))
pred.classes1 <- ifelse(prob1 > 0.5, 1, 0)

# Acurácia do modelo
obs.classes <- test.data$Churn
ac.1 <- mean(pred.classes1 == obs.classes)
ac.1


## Modelo com lambda.1se
modelo.2 <- glmnet(x,y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.1se)
coef(modelo.2)

AIC.2 <- sAIC(x=x, y=y, beta=coef(modelo.2), family="binomial")
AIC.2
BIC.2= AIC.2$AIC[1]-2*length(coef(modelo.2))+(length(coef(modelo.2))*log(length(train.data$Churn)))


# Fazendo a previsão para o dado de teste
prob2 <- exp(modelo.2 %>% predict(newx = x.test))/(1+exp(modelo.2 %>% predict(newx = x.test)))
pred.classes2 <- ifelse(prob2 > 0.5, 1, 0)

# Acurácia do modelo
ac.2 <- mean(pred.classes2 == obs.classes)
ac.2


## Modelo completo (logito)
modelo.3 <- glm(Churn ~., data = train.data, family = binomial("logit"))
summary(modelo.3)

AIC.3 <- modelo.3$aic
(BIC.3= modelo.3$aic-2*length(modelo.3$coefficients)+(length(modelo.3$coefficients)*log(length(train.data$Churn))))

# Fazendo a previsão para o dado de teste
prob3 <- modelo.3 %>% predict(test.data, type = "response")
pred.classes3 <- ifelse(prob3 > 0.5, 1, 0)

# Acurácia do modelo
ac.3 <- mean(pred.classes3 == obs.classes)
ac.3


## Modelo completo (probito)
modelo.4 <- glm(Churn ~., data = train.data, family = binomial("probit"))
summary(modelo.4)


(BIC.4= modelo.4$aic-2*length(modelo.4$coefficients)+(length(modelo.4$coefficients)*log(length(train.data$Churn))))

AIC.4 <- modelo.4$aic

# Fazendo a previsão para o dado de teste
prob4 <- modelo.4 %>% predict(test.data, type = "response")
pred.classes4 <- ifelse(prob4 > 0.5, 1, 0)

table(obs.classes)
table(pred.classes4)
summary(prob4)
# Acurácia do modelo
ac.4 <- mean(pred.classes4 == obs.classes)
ac.4


## Modelo reduzido
modelo.5 <- glm(Churn ~ ContractRenewal + CustServCalls + DataPlan+ RoamMins, data = train.data, family = binomial("logit"))
summary(modelo.5)

AIC.5 <- modelo.5$aic
(BIC.5= AIC.5-2*length(modelo.5$coefficients)+(length(modelo.5$coefficients)*log(length(train.data$Churn))))

# Fazendo a previsão para o dado de teste
prob5 <- modelo.5 %>% predict(test.data, type = "response")
pred.classes5 <- ifelse(prob5 > 0.5, 1, 0)

# Acurácia do modelo
ac.5 <- mean(pred.classes5 == obs.classes)
ac.5



resumo <- data.frame(prob1,prob2,prob3,prob4,prob5,pred.classes1,pred.classes2,pred.classes3,pred.classes4,pred.classes5,
                     ifelse(pred.classes1==pred.classes2 & pred.classes1==pred.classes3 &
                              pred.classes2==pred.classes3 & pred.classes1==pred.classes4 &
                              pred.classes1==pred.classes5 & pred.classes2==pred.classes4 &
                              pred.classes2==pred.classes5 & pred.classes3==pred.classes4 &
                              pred.classes3==pred.classes5 & pred.classes4==pred.classes5,0,1),obs.classes)
names(resumo) <- c("p1","p2","p3","p4","p5","class1","class2","class3","class4","class5","acuracia","obs")
head(resumo)

diff <- resumo %>% filter(acuracia==1)
diff

Modelo <- c("M1","M2","M3","M4","M5")
Acuracia <- c(ac.1,ac.2,ac.3,ac.4,ac.5)
deviance <- c(deviance(modelo.1),deviance(modelo.2),deviance(modelo.3),deviance(modelo.4),deviance(modelo.5))
AIC <- c(AIC.1$AIC,AIC.2$AIC,AIC.3,AIC.4,AIC.5)
pR2 <- round(c(0, 0, 
               1-(logLik(modelo.3)/logLik(fit.nulo)), 1-(logLik(modelo.4)/logLik(fit.nulo)),
                                                        1-(logLik(modelo.5)/logLik(fit.nulo))),3)
BIC = c(BIC.1, BIC.2, BIC.3, BIC.4, BIC.5)
R <- data.frame(Modelo,AIC,BIC,deviance,Acuracia, "Pseudo R²" = pR2)
R

qchisq(.95, length(train.data$Churn)-12)
##################################################################
##################################################################

# base heart disease.xlsx contem informaçoes de 4170 pacientes 
# de um hospital da Flórida. O objetivo é avaliar a probabilidade 
# de ocorrencia da doença arterial coronariana nos pacientes desse 
# hospital a partir de suas caracteristicas.

# male: indica se o paciente é do sexo masculino (1-masculino; 0-feminino);
# age: idade do paciente (em anos);
# smoker: indica se o paciente é fumante (1-fumante; 0-caso contrario);
# Churn: indica se o paciente tem Churn (1-diabético; 0-caso contrario);
# BMI: indice de massa corporal;
# CHD: indica se o paciente tem doença arterial coronariana.

# Lendo a base de base
base <- read_excel("heart_disease.xlsx")

# Ajustando o modelo
fit <- glm(CHD ~ male +
             age +
             smoker +
             Churn +
             BMI, 
           family=binomial(link = "logit"), data = base)


# Avaliando o ajuste (significância das variáveis, deviance e AIC)
summary(fit)

# Avaliando a adequabilidade do modelo a partir da ANOVA
anova(fit.1,test = "Chisq")

# Calculuando RC e seu intervalo de confiança de 95%
exp(cbind(OR = coef(fit.1), confint(fit.1, level=0.95)))[1,]


rc = exp(cbind(OR = coef(fit.1), confint(fit.1, level=0.95)))
rc = as.data.frame(rc)
colnames(rc) = c('Coeficientes',' Estimativa','2,5%', '97,5%')
kbl(as.data.frame(rc), caption= "Comparação de modelos gerados", booktabs = TRUE) %>%
  kable_paper("striped", "hover",full_width = F) %>%
  row_spec(0, bold = T, background = '#EDE2DB')%>%
  kable_styling(latex_options = "hold_position")

as.data.frame(summary(modelo.3))

as.data.frame(summary(modelo.3)$coefficients)
