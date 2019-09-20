#Estudiantes:
# Jose Barrera 15-10123
# Carlos Sivira 15-11377
###################################################
#Pregunta 1
poll <- read.table("CO3321_lab0.txt", header = TRUE)
attach(poll)
###################################################
#Pregunta 2
length(Sexo[Preg1 =="SI" & Sexo == "F"])
###################################################
#Pregunta 3
length(Edad[Edad > 40])
###################################################
#Pregunta 4
anwser4 = subset(poll, (Preg3 == "SI" & Preg5 == 1),select=Preg4)
mean(answer4$Preg4)
###################################################
#Pregunta 5
class(Sexo)
class(Edad)
class("F")
###################################################
