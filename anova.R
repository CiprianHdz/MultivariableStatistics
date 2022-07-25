#---------Uso de ANOVA---
#Taller: 28/02/22

#----- Se utiliza para evaluar los modelos. Por ejemplo, si agregando 
# o eliminando variables predictoras el ajuste del modelo tiene alguna significancia.

#Datos
file_url <- "http://www.bamlss.org/misc/rent99.raw"
data <- read.table(file_url, header = TRUE)
head(data)

#Comparación de modelos anidados.
lr1=lm(rent~area+yearc+location+bath+kitchen,data=data) #y=beta_0+ beta_i x_i
summary(lr1)

lr2=lm(rent~1,data=data) #y=beta_0 + error
summary(lr2)


#Comparación de los modelos
anova(lr1, lr2) #H_0: beta_i=0, H_1: Beta_i !=0 para algún i

#Como tenemos un p valor menor a 0.5 entonces rechazamos la hipótesis nula.  Existe evidencia para 
#decir que las variables año, area, locación, etc tienen significancia.


#Probar un solo predictor en particular por ejemplo año

lr1=lm(rent~area+yearc+location+bath+kitchen,data=data)
summary(lr1)

lr2=lm(rent~area+location+bath+kitchen,data=data)
summary(lr2)

anova(lr1, lr2) # H_0: beta_{yearc} =0 vs H_1: beta_{yearc} !=0

#p-valor pequeño. Entonces rechazamos la hipótesis nula. Existe evidencia para decir que la variable año tiene significancia.

#qué pasa si dos predictores son el mismo
lr1=lm(rent~area+yearc+location+bath+kitchen,data=data)
summary(lr1)

lr2=lm(rent~area+location+I(bath+kitchen),data=data)
summary(lr2)

anova(lr1, lr2) #H_0: beta_{bath}=beta_{kitchen} H_1: beta_{bath}!=beta_{kitchen}

#De nuevo se rechaza la hipótesis que beta_bath=beta_kitchen

#Probar que beta_area = 0.5
lr1=lm(rent~area+yearc+location+bath+kitchen,data=data)
summary(lr1)

lr2=lm(rent~offset(0.5*area)+location+bath+kitchen,data=data)
summary(lr2)

anova(lr1, lr2) # y= beta_0 + beta_1 x_1 + 0.5 x_2

#H_0: beta_{area}=0.5 H_1: beta_{area} != 0.5


#Ejercicio moral: Revisar cómo se hace la prueba con el estadístico F.