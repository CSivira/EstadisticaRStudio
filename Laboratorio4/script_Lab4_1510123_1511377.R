#Laboratorio 4. Modelos lineales y Anova
#Estudiantes:
# Jose Barrera  15-10123
# Carlos Sivira 15-11377

####################################################################
#Carga del archivo del proyecto
grades = read.table("calificaciones.txt", header = TRUE)
grades2 = read.table("calificaciones_prediccion.txt", header = TRUE)
####################################################################
#Pregunta 1

#############################Inicio inciso A########################
#Analisis descriptivo de los datos
# E6
E6 = grades$E6
summary(E6)
sd(E6)
boxplot(E6,main="Caja de E6",ylab="E6")

# E5
E5 = grades$E5
summary(E5)
sd(E5)
boxplot(E5,main="Caja de E5",ylab="E5")

# E4
E4 = grades$E4
summary(E4)
sd(E4)
boxplot(E4,main="Caja de E4",ylab="E4")

# E3
E3 = grades$E3
summary(E3)
sd(E3)
boxplot(E3,main="Caja de E3",ylab="E3")

# E2
E2 = grades$E2
summary(E2)
sd(E2)
boxplot(E2,main="Caja de E2",ylab="E2")

# E1
E1 = grades$E1
summary(E1)
sd(E1)
boxplot(E1,main="Caja de E1",ylab="E1")

# Y
Y = grades$Y
summary(Y)
sd(Y)
boxplot(Y,main="Caja de Y",ylab="Y")

#Plot de todas las variables en un mismo grafico
fac = c(replicate(60,"E6"), 
        replicate(60,"E5"), 
        replicate(60,"E4"), 
        replicate(60,"E3"), 
        replicate(60,"E2"), 
        replicate(60,"E1"), 
        replicate(60,"Y"))
#Generacion del factor para la grafica
fact = factor(fac)
dat = c(E6, E5, E4, E3, E2, E1, Y)
s = summary(grades)
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
matrix
#Es generada la grafica de todas las variables
boxplot(dat~fact,main="Caja de calificaciones",ylab="Resultados", xlab = "Exámenes")

#Se generan los histogramas respectivos
hist(E6,main="Histograma de E6",ylab="Frecuencia",xlab="E6")
hist(E5,main="Histograma de E5",ylab="Frecuencia",xlab="E5")
hist(E4,main="Histograma de E4",ylab="Frecuencia",xlab="E4")
hist(E3,main="Histograma de E3",ylab="Frecuencia",xlab="E3")
hist(E2,main="Histograma de E2",ylab="Frecuencia",xlab="E2")
hist(E1,main="Histograma de E1",ylab="Frecuencia",xlab="E1")
hist(Y,main="Histograma de Y",ylab="Frecuencia",xlab="Y")

#############################Inicio inciso B########################
#Matriz de Correlacion
variables_cuant = grades[1:7]
grades.cor = cor(variables_cuant)
cor(grades.cor)

#Matriz de correlacion grafica
#corrplot(grades.cor)
#corrplot(cor(grades.cor), method="color", tl.pos="n")

# Grafico de Dispersion
pairs(grades)

###########################Inicio inciso C y D#######################
#Se realiza la busqueda del mejor modelo lineal
###############Modelos Simples#################
ml1 = lm(Y~E1)
summary(ml1) 

ml2 = lm(Y~E2)
summary(ml2)

ml3 = lm(Y~E3)
summary(ml3)

ml4 = lm(Y~E4)
summary(ml4)

ml5 = lm(Y~E5)
summary(ml5)

ml6 = lm(Y~E6)
summary(ml6)

###############Modelos multiples#################
modelo1 = lm(Y~E6+E5+E4+E3+E2+E1)
summary(modelo1)

#Mejor modelo encontrado
modelo2 = lm(Y~E6+E5+E3+E2+E1)
summary(modelo2)
plot(modelo2, main = "Y~E6+E5+E3+E2+E1")

modelo3 = lm(Y~E6+E5+E3+E1)
summary(modelo3)

modelo4 = lm(Y~E6+E3+E1)
summary(modelo4)

#############################Inicio inciso E########################
#Prediccion de la variable Y
predict(modelo2, grades2, interval = "prediction")

#Analisis descriptivo de los residuos
summary(modelo2$residuals)
sd(modelo2$residuals)
par(mfrow = c(1,2))
boxplot(modelo2$residuals,main="Caja de residuales",ylab="Valores")
hist(modelo2$residual,main="Histograma de residuales",ylab="Frecuencia",xlab="Valores")

####################################################################
#Pregunta 2
#Se genera el vector de datos
m1 = c(15,16,14,15,17)
m2 = c(14,13,15,16,14)
m3 = c(13,12,11,14,11)
dat = c(m1,m2,m3)

#Se crea el factor del problema
fac = c(replicate(5,"m1"), replicate(5,"m2"), replicate(5,"m3"))
fact = factor(fac)

#Se revisa la media de cada metodo
tapply(dat, fact, mean)

#Se genera el diagrama de caja de los metodos
boxplot(dat~fact,main="Caja de métodos",ylab="Tiempos", xlab = "Métodos")

#Se crea el modelo lineal
mod.lm = lm(dat~fact)

#Se genera la tabla ANDEVA
anova(mod.lm)

#Se estudia la igualdad de las medias entre los metodos
pairwise.t.test(dat,fact)
####################################################################