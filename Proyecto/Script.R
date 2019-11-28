# Proyecto Final.
# Estudiantes:
# Miguel Colorado 14-10237
# Jose Barrera    15-10123
# Carlos Sivira   15-11377

####################################################################
#Cargando el archivo del proyecto
sales_pre = read.table("datosproy.txt", header = TRUE)
####################################################################
#Limpiando la data
sales = sales_pre[-c(23,67,122),]
####################################################################
#Pregunta 1
sales
summary(sales)

(149 + 30.5 + 23.3 + 467.6 + 1499.7)

((149 *100) / 2170.1)
((30.5 *100) / 2170.1)
((23.3 *100) / 2170.1)
((47.6 *100) / 2170.1)
((1499.7 *100) / 2170.1)

(((149 *100) / 2170.1) + 
((30.5 *100) / 2170.1) + 
((23.3 *100) / 2170.1) + 
((47.6 *100) / 2170.1) + 
((1499.7 *100) / 2170.1))

# Ventas
ventas = sales$ventas
summary(ventas)
sd(ventas)
boxplot(ventas,main="Caja de Ventas",ylab="Ventas")

# Facebook
facebook = sales$facebook
summary(facebook)
sd(facebook)    
boxplot(facebook,main="Caja de Facebook",ylab="Facebook")

# Periodico
periodico = sales$periodico
summary(periodico)
sd(periodico)
boxplot(periodico,main="Caja de Periodico",ylab="Periodico")

# Instagram
instagram = sales$instagram
summary(instagram)
sd(instagram)
boxplot(instagram,main="Caja de Instagram",ylab="Instagram")

# Tv
tv = sales$tv
summary(tv)
sd(tv)
boxplot(tv,main="Caja de Tv",ylab="Tv")

# Ebay
ebay = sales$ebay
summary(ebay)
sd(ebay)
boxplot(ebay,main="Caja de Ebay",ylab="Ebay")

# Region
Region = sales$Region
summary(Region)
sd(Region)
boxplot(Region,main="Caja de Region",ylab="Region")

#Desviaciones estandar
sd = c(sd(ventas), sd(facebook), sd(periodico), sd(instagram), sd(tv), sd(ebay), sd(Region));
sd
#Medias
mn = c(mean(ventas), mean(facebook), mean(periodico), mean(instagram), mean(tv), mean(ebay), mean(Region));
mn
#Coeficientes de variacion
cof = sd / mn
cof

s = summary(sales)
#Se calcula la desviacion estandar para cada una de las variables
sd = c(format(round(sd(E6), 2), nsmall = 2),
       format(round(sd(E5), 2), nsmall = 2),
       format(round(sd(E4), 2), nsmall = 2),
       format(round(sd(E3), 2), nsmall = 2),
       format(round(sd(E2), 2), nsmall = 2),
       format(round(sd(E1), 2), nsmall = 2),
       format(round(sd(Y), 2), nsmall = 2))
#Se genera la matris que contiene el resultado de summary y la desviacion estandar
matrix = rbind(s,sd)

fac = c(replicate(197,"v"), 
        replicate(197,"f"), 
        replicate(197,"p"), 
        replicate(197,"i"), 
        replicate(197,"t"), 
        replicate(197,"e"), 
        replicate(197,"r"))
#Generacion del factor para la grafica
fact = factor(fac)
dat = c(ventas, facebook, periodico, instagram, tv, ebay, Region)

#Es generada la grafica de todas las variables
#grid(nx=10, ny=NA)
#par(new=TRUE)
boxplot(dat~fact,main="Caja de calificaciones",ylab="Resultados", xlab = "Examenes")

# Histogramas
hist(ventas,main="Histograma de Ventas",ylab="Frecuencia",xlab="Ventas")
hist(facebook,main="Histograma de Facebook",ylab="Frecuencia",xlab="Facebook")
hist(periodico,main="Histograma de Periodico",ylab="Frecuencia",xlab="Periodico")
hist(instagram,main="Histograma de Instagram",ylab="Frecuencia",xlab="Instagram")
hist(tv,main="Histograma de Tv",ylab="Frecuencia",xlab="Tv")
hist(ebay,main="Histograma de Ebay",ylab="Frecuencia",xlab="Ebay")
hist(Region,main="Histograma de Region",ylab="Frecuencia",xlab="Region")

# Matriz de Correlacion
variables_cuant = sales[1:7]
sales.cor = cor(variables_cuant)
corrplot(datos.cor)

####################################################################
#Pregunta 2

#Calculo del intervalo de confianza para la media de ventas en la region 1
sales_per_region = sales$ventas[sales$Region == 1]
t.test(sales_per_region, conf.level = 0.95)$conf.int

#Calculo del intervalo de confianza para la media de ventas en la region 2
sales_per_region = sales$ventas[sales$Region == 2]
t.test(sales_per_region, conf.level = 0.95)$conf.int

#Calculo del intervalo de confianza para la media de ventas en la region 3
sales_per_region = sales$ventas[sales$Region == 3]
t.test(sales_per_region, conf.level = 0.95)$conf.int

#Calculo del intervalo de confianza para la media de ventas en la region 4
sales_per_region = sales$ventas[sales$Region == 4]
t.test(sales_per_region, conf.level = 0.95)$conf.int

####################################################################
#Pregunta 3

#1era posible variable independiente: facebook
ml1 = lm(sales$ventas ~ sales$facebook)
plot(sales$facebook, sales$ventas, main = "Ventas en función de la publicidad en Facebook", xlab = "Publicidad en Facebook", ylab = "Ventas")
abline(ml1)
summary(ml1)
plot(ml1, main = "Modelo 2.1")

#2da posible variable independiente: periodico
ml2 = lm(sales$ventas ~ sales$periodico)
plot(sales$periodico, sales$ventas, main = "Ventas en función de la publicidad en Periodico", xlab = "Publicidad en Periodico", ylab = "Ventas")
abline(ml2)
summary(ml2)
plot(ml2, main = "Modelo 2.2")

#3era posible variable independiente: instagram
ml3 = lm(sales$ventas ~ sales$instagram)
plot(sales$instagram, sales$ventas, main = "Ventas en función de la publicidad en Instagram", xlab = "Publicidad en Instagram", ylab = "Ventas")
abline(ml3)
summary(ml3)
plot(ml3, main = "Modelo 2.3")

#4ta posible variable independiente: tv
ml4 = lm(sales$ventas ~ sales$tv)
plot(sales$tv, sales$ventas, main = "Ventas en función de la publicidad en TV", xlab = "Publicidad en Television", ylab = "Ventas")
abline(ml4)
summary(ml4)
plot(ml4, main = "Modelo 2.4")

#5ta posible variable independiente: ebay
ml5 = lm(sales$ventas ~ sales$ebay)
plot(sales$ebay, sales$ventas, main = "Ventas en función de la publicidad en Ebay", xlab = "Publicidad en Ebay", ylab = "Ventas")
abline(ml5)
summary(ml5)
plot(ml5, main = "Modelo 2.5")

#6ta posible variable independiente: region
ml6 = lm(sales$ventas ~ sales$Region)
plot(sales$Region, sales$ventas, main = "Ventas en función de la Region", xlab = "Publicidad en FB", ylab = "Ventas")
abline(ml6)
summary(ml6)
plot(ml6, main = "Modelo 2.6")
####################################################################
#Pregunta 4
mlm1 = lm(sales$ventas ~ sales$facebook + sales$periodico + sales$instagram + sales$tv + sales$ebay + sales$Region)
summary(mlm1)
#plot(mlm1, main = "Modelo multiple 1")

mlm2 = lm(sales$ventas ~ sales$facebook + sales$instagram + sales$tv + sales$ebay + sales$Region)
summary(mlm2)
#plot(mlm2, main = "Modelo multiple 2")

mlm3 = lm(sales$ventas ~ sales$facebook + sales$instagram + sales$tv + sales$Region)
summary(mlm3)
#plot(mlm3, main = "Modelo multiple 3")

mlm4 = lm(sales$ventas ~ sales$facebook + sales$instagram + sales$Region)
summary(mlm4)
#plot(mlm4, main = "Modelo multiple 4")

#Mejor modelo consiguido. Presenta problemas en la normalidad de los residuales con datos atipicos
mlm5 = lm(sales$ventas ~ sales$facebook + sales$instagram)
summary(mlm5)
predict(mlm5,sales,interval = "prediction")
plot(mlm5, main = "Modelo multiple Y~facebook + instagram")
boxplot(rstandard(mlm5), main = "Caja de residuales", ylab = "Residuales")
hist(rstandard(mlm5), main="Histograma de residuales",ylab="Frecuencia",xlab="Rango")

#Se eliminan los valores de la tabla que afectan la normalidad de los residuales
sales_mod = sales[-c(128, 124, 56, 6, 77, 3, 74, 25, 186, 176, 171, 167, 164, 156, 129, 130, 101, 35, 80, 163, 133),]

#Esta modificacion del modelo anterior mejora el modelo a costa de aliminar datos de la tabla
#Se aprecia que los residuales se encuentran bien distribuidos de forma normal.
mlm6 = lm(sales_mod$ventas ~ sales_mod$facebook + sales_mod$instagram)
summary(mlm6)
predict(mlm6,sales_mod,interval = "prediction")
plot(mlm6, main = "Modelo multiple Y~facebook + instagram modificado")
boxplot(rstandard(mlm6), main = "Caja de residuales", ylab = "Residuales")
hist(rstandard(mlm6), main="Histograma de residuales",ylab="Frecuencia",xlab="Rango")

####################################################################
#Pregunta 5

#Se obtienen las ventas de la primera region
sales_per_region = sales$ventas[sales$Region == 1]
qqnorm(sales_per_region)
qqline(sales_per_region)
#Se define la hipotesis nula como m0 igual a 150
m0 = 150
#La hipotesis alternativa sera que m0 es mayor a 150

#Se obtiene el tama?o de la muestra (grande 49>30)
n = length(sales_per_region)
#Se obtiene la media de la muestra
sample_mean = mean(sales_per_region)
#Se obtiene la media de la muestra
standard_deviation = sd(sales_per_region)

#Se calcula el estadistico Z por tratarse de una muestra grande
z = ((sample_mean - m0) / (standard_deviation / sqrt(n)))
z
#Se obtiene el p-valor asociado a Z
p_value = pnorm(z, lower.tail=FALSE)
p_value

#Se realiza el estudio de las las hipotesis propuestas
t.test(sales_per_region, alternative = "greater", mu = 150, conf.level = 0.95)
####################################################################
#Pregunta 6

pesosNuevos <- data.frame(peso = c(43, 55, 70, 90, 65))
puntos<- data.frame(peso = seq(min(peso), max(peso), by = 0.5)) #Necesitamos para las bandas de confianza/predicción porque si no, se ven horribles y no se entiende nada
ml1 <- lm(estatura~peso)

#Valores predecidos para los puntos dados
predict(ml1, pesosNuevos, interval = "predict")

#Graficamos el modelo y las bandas de confianza/predicción para un 95% de confianza
plot(peso, estatura, xlab = "Peso de los estudiantes", ylab = "Estatura de los estudiantes", main = "Estatura de los estudiantes en función del peso")
abline(ml1)

#Graficamos los intervalos de prediccion
ip <- predict(ml1, puntos, interval = "predict")
lines(puntos$peso, ip[,2], lty = 2, col = "dodgerblue")
lines(puntos$peso, ip[,3], lty = 2, col = "dodgerblue") 

#Graficamos los intervalos de confianza
ic <- predict(ml1, puntos, interval = "confidence")
lines(puntos$peso, ic[,2], lty = 2)
lines(puntos$peso, ic[,3], lty = 2)
####################################################################
#Pregunta 7
####################################################################
#Pregunta 8
####################################################################