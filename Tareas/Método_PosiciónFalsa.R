# Mateo Galvis L�pez 
Fx <- function(x) exp(-x) + x -2
F1x <- function(x) 1-exp(-x)

# Halla la raiz de Fx

regula <- function(a,b) {
  x<-seq(a,b,0.1)
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,col="blue")
  error<-1
  conta<-0
  while (error > 1.e-4) {
    x<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) {b <- x}
    else {a <- x}
    error<-abs(Fx(x)/F1x(x))
    conta<-conta+1
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    cat("X=",x,"\t","E=",error,"\tIteraci�n:",conta,"\n")
  }
}
regula(0,3)
