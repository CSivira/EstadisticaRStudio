#Laboratorio 3. Pruebas de hipótesis y pruebas Chi-Cuadrado
#Estudiantes:
# José Barrera  15-10123
# Carlos Sivira 15-11377

####################################################################
#Pregunta 1
Actual <- c(300, 280, 344, 385, 372, 360, 288, 321, 376, 290, 301, 283)
Nuevo  <- c(276, 222, 310, 338, 200, 302, 317, 260, 320, 312, 334, 265)

#------------------------------Seccion 1------------------------------
#Se verifica si las varianzas pueden ser iguales
var.test(Actual, Nuevo)$conf.int
#------------------------------Seccion 2------------------------------
#Se cálcula el intervalo de confianza
t.test(Actual, Nuevo, paired=T, alternative = "two.sided")
####################################################################