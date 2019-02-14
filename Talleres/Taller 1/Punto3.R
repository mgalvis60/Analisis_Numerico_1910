#Código en Rstuido:
  # Halla la raiz de Fx
  Fx <- function(x) (2-2*cos(x))^2+(1-sin(x))^2
  F1x <- function(x) 8*sin(x)-3*sin(2*x)-2*cos(x) 
  
  newton <- function(x) {
    conta<-0
    for(i in 1:20) {
      x<-x-Fx(x)/F1x(x)
      if (Fx(x) == 0) break
      error<-abs(Fx(x)/F1x(x))
      conta<-conta+1
      cat("X=",x,"\t","E=",error,"\t","Iteraciones=",conta,"\n")
    }
  }
  newton(1)
  