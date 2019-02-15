#Mateo Galvis López

#Bisección en R Codigo 

#Biseccion
Fx <- function(x) exp(x)-pi*x

# Halla la raiz de Fx
biseccion <- function(a,b) {
  x<-seq(-3,3,0.001)
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,col="blue")
  x<-b
  d<-(a+b)/2
  i<-0
  error<-abs(a-b)/2
  while (error > 1.e-4) {
    i<-i+1
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) b <- x else {a <- x}
    d<-x
    x<-(a+b)/2
    #points(rbind(c(x,0)),pch=17,cex=0.7,col="red")
    text(x,0,i,cex=0.8,col="red")
    error<-abs(a-b)/2
    cat("X=",x,"\tE=",error,"\t\tIteración=",i,"\n")
  }
}

biseccion(-3,3)

