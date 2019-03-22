
#Taller interpolación 

#Punto 3

#Funcion 
Fx<-function(x)exp(x)

#Longitud para una partición regular
a<-0
b<-1
n<-10
l<-(b-a)/n


#Tabulación de datos apartir de la siguiente 
x<-c(seq(0,1,l))
y<-c()
for (i in 1:11) 
{
  y[i]<-(Fx(x[i]))
}
print(y[1:11])
#Parte A

#Graficas
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="Eje x", ylab="Eje y", main="exponencial de x")
curve(plot,add=T,from =5,to =25) 

#Parte B

#Método de Lagrnage

DatosX=x[1:10]
DatosY=y[1:10]
polinomioAjustado=poly.calc(DatosX,DatosY)
polinomioAjustado
plot(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1)
curve(polinomioAjustado,add=T,from =0,to =5) 

#Método de baricéntrica de Lagrange

barylag(x[c(1:10)], y[c(1:10)],c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.8,0.9))

#Método de Newton: Diferencias divididias



