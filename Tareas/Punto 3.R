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

#Graficas
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="Eje x", ylab="Eje y", main="exponencial de x")
#curve(plot,add=T,from =5,to =25) 
