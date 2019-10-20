#Laboratorio 2. Intervalos de Confianza
#Estudiantes:
# José Barrera  15-10123
# Carlos Sivira 15-11377

par(mfrow=c(1,3))

####################################################################
#Pregunta 1

#Almacenamos los datos a analizar
data1 <- c(6.9, 7.6, 6.5, 6.2, 5.3,
           7.8, 7.0, 5.5, 7.6, 6.7,
           7.3, 6.6, 7.1, 6.9, 6.0,
           6.8, 6.5, 7.2, 5.8, 8.6,
           7.6, 7.1, 6.0, 7.2, 7.7)

#Datos para el análisis descriptivo
summary(data1)

#Cálculo del coeficiente de variación
sd(data1) / mean(data1)

#Graficamos para apoyar nuestro análisis
boxplot(data1, 
        ylab = 'Horas', 
        main='Horas de Sueño', 
        col = 'green')

hist(data1, 
        ylab = 'Horas', 
        main='Horas de Sueño', 
        col = 'green')

qqnorm(data1, main='Horas de sueño')
qqline(data1)

#Calculamos el intervalo de confianza
#de 78% para la media
t.test(data1, conf.level = 0.78)$conf.int

####################################################################
#Pregunta 2
#Datos del problema 2
data2 <- c(15, 16, 14, 15, 17, 18, 19, 15, 13, 12, 11, 13, 11, 9, 10, 10)
#Definición de la matriz y sus respectivos headers por columna
dataMatrix <- matrix(data2, nrow=8)
colnames(dataMatrix) <- c("Método I", "Método II")

#------------------------------Seccion 1------------------------------
#Obtención del summary
summary(dataMatrix)
#Cálculo del coeficiente de variación para cada columna
sd(dataMatrix[, 1]) / mean(dataMatrix[,1])
sd(dataMatrix[, 2]) / mean(dataMatrix[,2])
#Muestra de las gráficas
par(mfrow=c(3,2))
boxplot(dataMatrix, ylab = 'Tiempos', main='Tiempos por ciclistas ambos métodos', col = 'green')
hist(dataMatrix, ylab = 'Cantidad de ciclistas', main='Tiempos por ciclistas ambos métodos', col = 'green')
hist(dataMatrix[,1], ylab = 'Cantidad de ciclistas', main='Tiempos por ciclistas Método I', col = 'green')
hist(dataMatrix[,2], ylab = 'Cantidad de ciclistas', main='Tiempos por ciclistas Método II', col = 'green')
qqnorm(dataMatrix[,1], main='Tiempos por ciclistas Método I')
qqline(dataMatrix[,1])#Metodo 1
qqnorm(dataMatrix[,2], main='Tiempos por ciclistas Método II')
qqline(dataMatrix[,2])#Metodo 2

#------------------------------Seccion 2------------------------------
#Se verifica si las varianzas pueden ser iguales
var.test(dataMatrix[,1], dataMatrix[,2])$conf.int
#Se cálcula el intervalo de confianza
t.test (dataMatrix[,1], dataMatrix[,2], var.equal = TRUE, conf.level = 0.95 )$conf.int

#------------------------------Seccion 3------------------------------
#Cálculo de la diferencia de las varianzas.
var(dataMatrix[,1]) - var(dataMatrix[,2])
####################################################################