#Punto 1
#Ecuación diferencial 
# X´´- 0.5 X - X´ = 0 ; X(0)= 2, X´(0)= -1

#Punto 1 

#a) Para h = 0.1 

#Solución por métodos númericos
ecuaciones<- function(t,y,parms)
{
  dx = y[2]
  dz = 0.5* y[1]+y[2]
  return(list (c(dx,dz)))
}
#Solución Exacta 
solucionExacta<- function(x)
{
  p<-exp(-0.366025*x)*(2.1547-0.154701*exp(1.73205*x))
  return(p)
}


#h = 0.1
x1 = seq(0,2, by=0.1)
sol1 = ode(c(2,-1), x1,ecuaciones,parms=NULL, method="rk4")
tabla = cbind(x1,sol1[,2])
tabla
plot(x1,sol1[,2],xlab="Eje x", ylab="Eje y", main="Método de Runge-Kutta vs Solución exacta de la Ecuación Diferencial: X´´- 0.5 X - X´ = 0 con h1= 0.1 " )


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
dev.off()


#b) Para h= 0.2 

x2 = seq(0,4, by=0.2)
sol3 = ode(c(2,-1), x2,ecuaciones,parms=NULL, method="rk4")
tabla = cbind(x2,sol3[,2])
tabla
plot(x2,sol3[,2],xlab="Eje x", ylab="Eje y", main="Método de Runge-Kutta vs Solución exacta de la Ecuación Diferencial: X´´- 0.5 X - X´ = 0 con h2= 0.2 " )

a3 = poly.calc(x2, sol3[,2])
curve(a3,add=T)
abline(h=0,v=0,col="red")
#Solución exacta 

#Solución exacta 
sol4<- c()
for(i in 1:length(x2))
{
  sol4[i]<- solucionExacta(x2[i]);
}
sol4
lines(x2,sol4,col="blue")
error<-c()

for(i in 1: length(x2))
{
  error[i]<- abs((sol4[i]-sol3[i])/sol4[i])
}

error



dev.off()





