#Ecuación diferencial 
# X´´- 0.5 X - X´ = 0 


#Solución Exacta 

fx1<-function(t) ( exp(-0.366025*t) )*( (exp(1.73205 * t) ) +1 )  

paso<-c (seq(0,1,0.1) )
y<- c()

for(i in 1:11)
{
  y[i]<- fx1(x[i])
}

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="Eje x", ylab="Eje y", main="Solución Exacta de la Ecuación Diferencial")

#Solución por métodos Númericos
fx2<- function(t)(6t)

sol = ode(c(2,-1),paso,  )