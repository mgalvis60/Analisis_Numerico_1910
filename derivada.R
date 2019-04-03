h<-c()

derivada<-function(a,h)
{
 for(i in 1:length(h))
 resultado<-(sin(a+h)-sin(h))/h
 return(resultado)
}
errorFuncion<-function(a,resultado)
{
  for(i in 1:length(h))
  error<-abs(resultado-cos(a))
  return(error)
}
  
h<- seq(0.1, 0.6,len=(6))

resultado=derivada(0.5,h)
error=errorFuncion(0.5,resultado)
tabla<-matrix(c(h,resultado,error),ncol = (length(resultado)), byrow = TRUE)
colnames(tabla)<-c("Datos 1","Datos 2","Datos 3","Datos 4","Datos 5","Datos 6")
rownames(tabla)<-c("h","resultado","Error")
tabla
