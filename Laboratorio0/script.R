#Estudiantes:
# Jose Barrera  15-10123
# Carlos Sivira 15-11377
####################################################################
#Pregunta 1
poll <- read.table("CO3321_lab0.txt", header = TRUE)
####################################################################
#Pregunta 2
length(poll$Sexo[poll$Preg1 =="SI" & poll$Sexo == "F"])
####################################################################
#Pregunta 3
length(poll$Edad[poll$Edad > 40])
####################################################################
#Pregunta 4
mean(subset(poll, (Preg3 == "SI" & Preg5 == 1),select=Preg4)$Preg4)
####################################################################
#Pregunta 5
class(poll$Sexo)
class(poll$Edad)
####################################################################
