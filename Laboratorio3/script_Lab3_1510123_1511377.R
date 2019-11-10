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
#Se c�lcula el intervalo de confianza
t.test (Actual, Nuevo, var.equal = TRUE, conf.level = 0.93, alternative = "greater")
####################################################################
#Pregunta 2
#Datos del problema
alpha <- 0.05
fi <- c(35,99,104,110,62,25,13)
mi <- c(0,1,2,3,4,5,6)
n <- sum(fi)
k <- length(fi)
lambda <-  2.435
#No se pierden grados de libertad, por lo tanto r = 0
r <- 0
#Calculo del estadistico chi cuadrado
pi <- (((lambda^mi)*(exp(-lambda))) / factorial(mi))
pi[k] = 1 - (pi[1] + pi[2] + pi[3] + pi[4] + pi[5] + pi[6])
en <- (n * pi)
chi_square <- sum((fi - en)^2 / en)
#Se obtiene el valor de chi cuadrado para el alpha dado
chi_value <- qchisq(1 - alpha, k - 1 - r)
#Se obtiene el p-valor asociado
p_value <- pchisq(chi_square, k - 1 - r)
#Se muestra el valor del estadistico chi cuadrado
chi_square
#Se muestra el valor del chi para el alpha dado
chi_value
#Se muerta el resultado obtenido del p-valor asociado al estadistico chi cuadrado
p_value
####################################################################
#Pregunta 3
Oscura <- c(31, 26, 18)
Clara  <- c(19, 40, 31)
Ligera <- c(10, 15, 22)

matriz <- cbind(Oscura, Clara, Ligera)

chisq.test(matriz)
####################################################################