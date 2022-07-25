
#Primero haremos uso de las bases de datos disponibles en R
library(datasets)

#La siguiente es una base de datos que contiene información sobre los pétalos
#para tres clases de iris (flores) Setosa/Versicolor/Viginica
data(iris)

#Mostramos algunas estadísticas descriptivas
summary(iris)

#Ahora realizamos el diagrama de dispersión para comparar la anchura y longitud de los pétalos
plot(iris$Petal.Length,iris$Petal.Width)

#Para agregar etiquetas basta hacer lo siguiente
plot(iris$Petal.Length,main='Información sobre pétalos',
     xlab="índice",ylab="Ancho del pétalo")

#Definamoa los vectores de observaciones
x=iris$Petal.Length
y=iris$Petal.Width

#Construimos la matriz de datos de la siguiente forma
X=cbind(x,y)
print(X)

#Graficamos
plot(x,y)

#Histograma 
hist(iris$Sepal.Length)

#-----MEDIA-------------

#Manualmente:
prom_vec=c(mean(x),mean(y))
prom_vec

#Graficamos la matriz de datos y su media
plot(x,y)
par(new=TRUE)
plot(mean(x),mean(y),col='red',lwd=4)


#Otras formas de obtener el vector de mediases haciendo uso de la función apply
apply(X,2,mean) #2 es para columnas

#Ejercicio: Calcular la media mediante la fórmula 1/n X'1


#---------MATRIZ DE VARIANZA COVARIANZA---------
cov(X)

#Otra forma de calcular la matriz de covarianza (Manualmente)
n=nrow(X)
C=diag(n)-matrix(1/n,n,n)
Xc=C%*%X
S=t(Xc)%*%Xc/(n-1)
S
#Notamos entonces que las variables tienen corración positiva
#entre el largo del pétalo y la anchura


#Matriz de correlación 
n=nrow(X)
C=diag(n)-matrix(1/n,n,n)
D=diag(apply(X,2,sd))
Xs=C%*% X %*% solve(D)
R=t(Xs) %*% Xs/(n-1)
R

cor(X)


#Podemos también colorear la matriz de corración 
library(RColorBrewer)
data(mtcars)
M=as.matrix(mtcars)
M
cmat = cor(M)
cmat
heatmap(cmat,Rowv = NA, Colv = NA,col=colorRampPalette(c("blue", "white", "red"))(20))
#Negative correlations are shown in blue and the positive ones in red.

#Luego vemos ggplot

####----------Manejo de 3d plots---------
X=matrix(c(12,17,29,18,20,38,14,16,30,20,18,38,16,19,35),ncol = 3)
print(X)

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
scatterplot3d(X,)

#http://users.stat.umn.edu/~helwig/notes/datamat-Notes.pdf
