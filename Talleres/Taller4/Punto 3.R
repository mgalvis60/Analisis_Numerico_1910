# Punto 3
#Ecuación Diferencial 
# Y´´-Y'- X+Y+1 = 0;  Y(0)= 1, Y´(0)= 2;

#Para h = 0.1

#Solución por métodos Númericos
ecuaciones<- function(t,y,parms)
{
  dy = y[2]
  dz = y[2]-y[1]-1
  return(list (c(dy,dz)))
}

#Solución Exacta 
solucionExacta<- function(x)
{
  p<- x + ( exp(x/2)*sin((sqrt(3)*x)/2) )/sqrt(3) + exp(x/2) * cos((sqrt(3)*x)/2)
  
  return(p)
}


#h = 0.1
x1 = seq(0,2, by=0.1)
sol1 = ode(c(1,2), x1,ecuaciones,parms=NULL, method="rk4")
tabla = cbind(x1,sol1[,2])
tabla
plot(x1,sol1[,2],xlab="Eje x", ylab="Eje y", main="Método de Runge-Kutta vs Solución exacta de: Y´´-Y'- X+Y+1 = 0;  Y(0)= 1, Y´(0)= 2 ; con h1= 0.1 " )


a1 = poly.calc(x1, sol1[,2])
curve(a1,add=T)
abline(h=0,v=0,col="red")
print(a1)

#Solución exacta 
sol2<- c()
for(i in 1:length(x1))
{
  sol2[i]<- solucionExacta(x1[i]);
}
sol2
lines(x1,sol2,col="blue")

for(i in 1: length(x1))
{
  error[i]<- abs((sol2[i]-sol1[i])/sol2[i])
}

error


dev.off()
