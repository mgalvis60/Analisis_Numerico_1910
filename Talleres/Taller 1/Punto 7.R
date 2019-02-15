#Función 
fx<-function(x)exp(x)-pi

#Main
raiz<-function(a,b,E)
{
  conta<-1
  error<-1
  x1<-a
  d<-(b-a)/10
  while(d>E)
  {
    d<-(b-a)/10
    x2<-d
    b<-d
    if(fx(x1)*fx(x2)<0)
    {
      x2<-x1-d
      d<-d/10
    }
    error<-abs(a-b)/2
    cat("X=",x1,"\tE=",error,"\t\tIteración=",conta,"\n")
    x1<-x2
    conta<-conta+1
  }
}
raiz(0,2,1.e-8)

