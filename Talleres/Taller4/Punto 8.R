# Punto 2
#Conjunto de ecuaciones diferenciales 

#Solución por métodos númericos
ecuaciones<- function(t,y,parms)
{
  with(as.list(y),{
    
    dx1 = 1*X-pi*Y
    dx2 = 3*X -7*Y
    return(list (c(dx1,dx2)))
  })
}
x1 <- seq(0,2, by=0.1)
datos <- c(X=-1,Y=2)
sol1 = ode(datos, x1,ecuaciones,parms=NULL, method="rk4")


tabla = cbind(x1,sol1[,2])
tabla
plot(x1,sol1[,2],xlab="Eje x", ylab="Eje y", main="Método de Runge-Kutta vs Solución exacta de la Ecuación Diferencial: X´´- 0.5 X - X´ = 0 con h1= 0.1 " )


a1 = poly.calc(x1, sol1[,2])
curve(a1,add=T)
abline(h=0,v=0,col="red")
print(a1)

