####################################################################
#Cargando el archivo del proyecto
sales_pre = read.table("datosproy.txt", header = TRUE)
####################################################################
#Limpiando la data
sales = sales_pre[-c(23,67,122),]
####################################################################
#Pregunta 3
# Encuentre el modelo de regresion simple que mejor se ajuste a
# los datos; realice las pruebas estadisticas que considere conveniente
# para justificar su respuesta, incluyendo un analisis de residuales.

# Facebook
modeloFacebook = lm(ventas~facebook)
summary(modeloFacebook)
plot(modeloFacebook, main = "Ventas ~ Facebook")

# Periodico
modeloPeriodico = lm(ventas~periodico)
summary(modeloPeriodico)
plot(modeloPeriodico, main = "Ventas ~ Periodico")

# Instagram
modeloInstagram = lm(ventas~instagram)
summary(modeloInstagram)
plot(modeloInstagram, main = "Ventas ~ Instagram")

# Tv
modeloTv = lm(ventas~tv)
summary(modeloTv)
plot(modeloTv, main = "Ventas ~ Tv")

# Ebay
modeloEbay = lm(ventas~ebay)
summary(modeloEbay)
plot(modeloEbay, main = "Ventas ~ Ebay")

# Region
modeloRegion = lm(ventas~Region)
summary(modeloRegion)
plot(modeloRegion, main = "Ventas ~ Region")

####################################################################
#Pregunta 6
# Para el modelo de regresión lineal simple obtenido en el
# inciso 3, realice la predicción correspondiente para 5 ventas
# que se anexan a la muestra, los datos se presentan en el Cuadro 1.
# Grafique los intervalos de predicción y de confianza
# respectivamente. Realice el análisis respectivo.

# El mejor modelo fue modeloInstagram, entonces se utilizara este
# para predecir las ventas.
modeloFacebook = lm(ventas~facebook)

nuevosPresupuestos = data.frame(presupuestos=c(52, 58, 63, 79, 81))

# Se grafican los datos
plot(facebook, ventas)
abline(modeloFacebook)

# Predicción para el nuevo presupuesto asignado a publicidad
(predict(modeloFacebook, nuevosPresupuestos, interval = 'predict'))

# Se generan puntos para las bandas
sequence = data.frame(facebook = seq(0, 300, 1))

# Intervalo de predicción
predicFacebook = predict(modeloFacebook, sequence, interval = "prediction")
lines(sequence$facebook, predicFacebook[,2], lty = 2, col = "red")
lines(sequence$facebook, predicFacebook[,3], lty = 2, col = "red")

# Intervalo de confianza para el 95%
confFacebook = predict(modeloFacebook, sequence, interval = "confidence")
lines(sequence$facebook, confFacebook[,2], lty = 2, col = "blue")
lines(sequence$facebook, confFacebook[,3], lty = 2, col = "blue")

####################################################################