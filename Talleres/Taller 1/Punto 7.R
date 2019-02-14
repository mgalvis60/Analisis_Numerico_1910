#Función 
fx<-function(x)exp(x)-pi

#Main
raiz<-function(a,b,E)
{
  error<-1
  x1<-a
  while(error<E)
  {
    i<-i+1
    d<-(b-a)/10
    x2<-d
    if(f(x1)*fx(x2)<0)
    {
      x2<-x2-d
      d<-d/10
    }
    error<-error(fx1)/error(fx2)
    x1<-x2
    cat("X=",x,"\tE=",error,"\t\tIteración=",i,"\n")
  }
  
  
}
raiz(0,1,0.000000001)

