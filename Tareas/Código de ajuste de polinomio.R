#install.packages("Matrix")#instalar paquete
#install.packages("PolynomF")#instalar paquete
library(Matrix)
library(PolynomF)

# Dado los siguientes vectores 

x=c(6,8,10,12,14,16,18,20)
y=c(7,9,12,18,21,19,15,10)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="Hora", ylab="Grados", main="Horas vs Grados, sin ajuste")
DatosX = x[1:8]; DatosY = y[1:8]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio

#Datos del polinomio
x1<-Ajuste_Polinomio(x)
y1<-Ajuste_Polinomio(y)

#Ajuste
plot(x1,y1, pch=19, cex=1, col = "blue", asp=1,xlab="Hora", ylab="Grados", main="Horas vs Grados, con ajuste")
curve(Ajuste_Polinomio,add=T,from =5,to =25)

#Calculó de  errores
error<-c()
 for (i in 1:8) 
 {
   error[i]<-(abs(y[i]-y1[i])/y[i])
 }
#Impresion de resultados
tabla<-matrix(c(x,y,y1,error),ncol = 8, byrow = TRUE)
colnames(tabla)<-c("Datos 1","Datos 2","Datos 3","Datos 4","Datos 5","Datos 6","Datos 7","Datos 8")
rownames(tabla)<-c("X","Y","Y1","Error")
tabla<-as.table(tabla)
cat("El Polinomio obtenido es: ")
print(Ajuste_Polinomio)
tabla


