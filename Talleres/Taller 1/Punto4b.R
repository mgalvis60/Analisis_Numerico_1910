#Mateo Galvis López

Secante
Fx <- function(x) cos(3*x)+exp(x)
F1x <- function(x) -3*sin(3*x)+exp(x)


# Metodo de la Secante

# Halla la raiz de Fx

secante <- function(x0,x1) {
  x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
  conta<-0
  error <-1
  while (error > 1.e-4) {
    x0<-x1
    x1<-x
    x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
    if (Fx(x) == 0) break
    error<-abs(Fx(x)/F1x(x))
    conta<-conta+1
    cat("X=",x,"\t","E=",error,"\tIteración:",conta,"\n")
  }
}
secante(0,3)
