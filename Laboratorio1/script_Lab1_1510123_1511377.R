#Laboratorio 1. Estadística descriptiva
#Estudiantes:
# Jose Barrera  15-10123
# Carlos Sivira 15-11377

#Guardamos la data en la variable poll
poll <- read.table("CO3321_lab1.txt", header = TRUE)

####################################################################
#Pregunta 1

#par(mfrow=c(5,2))

#Para cada columna calculamos min, max, cuartiles, mediana,
#desviacion estandar (si aporta informacion) y una grafica

#Sexo
summary(poll$Sexo)
#barplot(table(poll$Sexo),
#        ylab = 'N° personas',
#        main='N° personas por sexo',
#        col = 'aquamarine2')

#Edad
summary(poll$Edad)
sd(poll$Edad)
#boxplot(poll$Edad, 
#        ylab = 'Edad', 
#        main='Edad por persona', 
#        col = 'green')

#Preg1
summary(poll$Preg1)
#barplot(table(poll$Preg1),
#        ylab = 'N° personas', 
#        main='Aprobacion del precio del metro',
#        col = 'darkgoldenrod2')

#Preg2
summary(poll$Preg2)
sd(poll$Preg2)
#boxplot(poll$Preg2, 
#        ylab = 'Precio', 
#        main='Precio sugerido boleto', 
#        col = 'yellow')

#Preg3
summary(poll$Preg3)
#barplot(table(poll$Preg3), 
#        ylab = 'N° personas', 
#        main='Personas que compran a los buhoneros', 
#        col = 'chocolate1')

#Preg4
summary(poll$Preg4)
sd(poll$Preg4)
#boxplot(subset(poll,
#               (Preg3 == 'SI'),
#               select=Preg4), 
#        ylab = 'Gasto', 
#        main='Gasto semanal en buhoneros', 
#        col = 'coral2')

#Preg5
summary(poll$Preg5)
#barplot(table(poll$Preg5),
#        ylab = 'N° personas',
#        main='N° personas por linea de metro',
#        col = 'blueviolet')

####################################################################
#Pregunta 2

#boxplot(split(poll$Edad, poll$Sexo), 
#        ylab = 'Edad', 
#        main='Edad según sexo', 
#        col = 'red')

####################################################################
#Pregunta 3

posAns4 = subset(poll, (Preg3 == 'SI'), select=Preg4)$Preg4
posAns5 = subset(poll, (Preg3 == 'SI'), select=Preg5)$Preg5

#boxplot(split(posAns4, posAns5), 
#        ylab = 'Gasto en buhoneria', 
#        main='Gasto en buhoneria según línea de metro', 
#        col = 'blue')