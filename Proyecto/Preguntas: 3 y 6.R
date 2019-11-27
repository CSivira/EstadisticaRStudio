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
modeloInstagram = lm(ventas~instagram)

nuevosPresupuestos = data.frame(pie=c(52, 58, 63, 79, 81))

# Se grafican los datos
plot(instagram, ventas)
abline(modeloInstagram)

# Predicción para el nuevo presupuesto asignado a publicidad
(predict(modeloInstagram, nuevosPresupuestos, interval = 'predict'))

# Se generan puntos para las bandas
sequence = data.frame(instagram = seq(0, 50, 1))

# Intervalo de predicción
predicInstagram = predict(modeloInstagram, sequence, interval = "prediction")
lines(sequence$instagram, predicInstagram[,2], lty = 2, col = "red")
lines(sequence$instagram, predicInstagram[,3], lty = 2, col = "red")

# Intervalo de confianza para el 95%
confInstagram = predict(modeloInstagram, sequence, interval = "confidence")
lines(sequence$instagram, confInstagram[,2], lty = 2, col = "blue")
lines(sequence$instagram, confInstagram[,3], lty = 2, col = "blue")

####################################################################