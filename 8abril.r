

#Datos
library(ISLR2)
College1 = College[,-c(3,4)]
View(College1)


#Ajuste un modelo lineal multivariado en todo el conjunto de datos y realice un análisis de colinealidad.
#¿es necesario remover alguna variable del análisis?

#Ajuste del modelo
lr= lm(Apps ~ .,data = College1)
summary(lr)

#Correlación
install.packages("devtools")  # if not already installed
library(devtools)

install_git("https://github.com/ccolonescu/PoEdata")


library(corrplot)
#Cambiar Yes/No para 0/1 sea numérico
College1$Private <- ifelse(College1$Private == "Yes", 1 ,0)
M = cor(College1)
corrplot(M,method = "number",number.cex = 0.5)

#VIF
install.packages('caret')
library(caret)

vif_values = vif(lm_college)
barplot(vif_values, main = "Valores VIF", 
        horiz = TRUE,col = "magenta",las=1,xlim = c(0,11),cex.names=0)
abline(v = 10, lwd = 3, lty = 2)

# Top 10 y top 25 tienen alta correlación y son las que tienen 
#el vif más alto. Requieren más información.

#b)Construya un conjunto de datos de entrenamiento considerando desde el registro 1 hasta
#el 600 y un conjunto de datos de prueba desde la observación 601 hasta la 777.

#Training 
train = College1[c(1:600),]
  #Test
  test= College1[c(601:777),]


library(leaps)
mejor_subconjunto <- regsubsets(Apps ~ ., data=train, nvmax=15)
sub_summary <- summary(mejor_subconjunto)
sub_summary


#Mejor subconjunto
plot(sub_summary$cp, xlab = "Numero de Variables",ylab = "Cp", type = "l")
minimo_cp <- which.min(sub_summary$cp)
points (minimo_cp, sub_summary$cp[minimo_cp] , col = "red", cex = 2, pch = 20)

which.min(sub_summary$cp)

mod_cp <- lm(Apps~Private + Top10perc +
               F.Undergrad + Room.Board + perc.alumni + Expend + Grad.Rate,data=College1)
             summary(mod_cp)
             
#RIDGE
X <- model.matrix(Apps~.,data=train)[,-1]
Y <- train$Apps 

library(glmnet)

grid <- 10^seq(2, -6, length = 100)
cv.ridge <- cv.glmnet(X, Y, alpha = 0, lambda = grid)
mejor_lambda_ridge <- cv.ridge$lambda.min
mod_Ridge <- glmnet(X, Y, alpha = 0, lambda = mejor_lambda_ridge)
#Se despliegan coeficientes y mejor lamda
mod_Ridge$beta
summary(mod_Ridge)

cv.lasso <- cv.glmnet(X, Y, alpha = 1, lambda = grid)
#Se calcula mejor lambda
mejor_lambda_lasso <- cv.lasso$lambda.min
mod_lasso <- glmnet(X, Y, alpha = 1, lambda = mejor_lambda_lasso)
#Se despliegan coeficientes y mejor lamda
mod_lasso$beta


X_test <- college_test[,-c(2)]
Y_test <- college_test$Apps

cp_pred = predict(mod_cp,X_test)
mean((cp_pred - Y_test)ˆ2)

X_test1 <- model.matrix(Apps~.,data=college_test)[,-1]
ridge_pred = predict(mod_Ridge, s = mejor_lambda_ridge, newx = X_test1)
mean((ridge_pred - Y_test)ˆ2)

lasso_pred = predict(mod_lasso, s = mejor_lambda_lasso, newx = X_test1)
mean((lasso_pred - Y_test)ˆ2)


