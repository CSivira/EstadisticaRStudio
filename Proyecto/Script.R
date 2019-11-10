#Proyecto. Estad?stica descriptiva
#Estudiantes:
# Jose Barrera  15-10123
# Carlos Sivira 15-11377
# Miguel

####################################################################
#Cargando el archivo del proyecto
sales = read.table("datosproy.txt", header = TRUE)
sales[,]
####################################################################
#Pregunta 1

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


####################################################################
#Pregunta 4


####################################################################
#Pregunta 5

#Se obtienen las ventas de la primera region
sales_per_region = sales$ventas[sales$Region == 1]

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
#z = ((sample_mean - m0) / (standard_deviation / sqrt(n)))

#Se obtiene el p-valor asociado a Z
#p_value = pnorm(Z, lower.tail=FALSE)
#p_value

#Se realiza el estudio de las las hipotesis propuestas
#t.test(sales_per_region, alternative = "upper", mu = 150, conf.level = 0.99)
####################################################################
#Pregunta 6


####################################################################
