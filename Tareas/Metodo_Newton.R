# Halla la raiz de Fx
Fx <- function(x) exp(x)-pi
F1x <- function(x) exp(x)

grafico<- function(x)
{
  x<-seq(-3,3,0.01)
  plot(x,Fx(x),type="l",col="blue",
  main ="Gráfico por el Método de Newton",
  xlab = "Valor de X",ylab="Valor de F(x)")
  abline(h=0,col="red")
}
# Código por el Método de Newton
newton <- function(x)
{ conta<-0
  for(i in 1:20) 
  {
    x<-x-Fx(x)/F1x(x)
    if (Fx(x) == 0) break
    error<-abs(Fx(x)/F1x(x))
    conta<-conta+1
    cat("X=",x,"\t","E=",error,"\tIteración=",conta,"\n")
  }
}
newton(1)
grafico(0)

