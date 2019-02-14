evaluarFuncion<-function(f, x){
  return(eval(f))
}


CalcularRaiz<-function(n, a, x, pre){
  y=parse(text=paste("x^",n,"-",a))
  dy<-D(y,"x")
  
  it<-1
  
  
  while(TRUE){
    x<-x-(evaluarFuncion(y, x)/evaluarFuncion(dy, x))
    
    cat ("|", it, "|",x, "|", evaluarFuncion(y,x),"|","\n")
    if (abs(evaluarFuncion(y, x))<=pre)   break
    it<-it+1
  }
  
  cat ("La raiz n-esima (n=",n,") de ",a," es ",x," con un total de ",it, " iteraciones.", "\n")
  
}

CalcularRaiz(n = 4,a = 256,x = 5,pre = 0.0001)