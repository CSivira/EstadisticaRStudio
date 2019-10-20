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

sd(data1)

#Graficamos para apoyar nuestro análisis
boxplot(data1, 
        ylab = 'Horas', 
        main='Horas de Sueño', 
        col = 'chocolate1')

hist(data1, 
        ylab = 'Horas', 
        main='Horas de Sueño', 
        col = 'aquamarine2')

qqnorm(data1, main='Horas de Sueño')
qqline(data1)

#Calculamos el intervalo de confianza
#de 78% para la media
t.test(data1, conf.level = 0.78)$conf.int

####################################################################
#Pregunta 2

data2 <- c(15, 16, 14, 15, 17, 18, 19, 15, 13, 12, 11, 13, 11, 9, 10, 10)

dataMatrix <- matrix(data2, nrow=8)
colnames(dataMatrix) <- c("Método I", "Método II")
dataMatrix

####################################################################