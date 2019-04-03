#Varibales 
x<-c()
y<-c()
Error<-c()
yCalculado<-c()
ErrorTotal<-0
operaciones<-0



#PARTE SUPERIOR 
x<-c(1,1.8,3.5,5, #4Puntos
     6.3,8.25,7,10, #8Puntos
     12.5, 15.4, 17, #11Puntos
     18.4,20.5,21.4,23,24, #16Puntos
     26,28, #18Puntos
     28.6,30, #20Puntos
     
     #PARTE INFERIOR
     1.01,6.45,
     7,7.44,7.86,8,7.74,7.56,8.6,10.4,11.97,
     12.9,13.63,14.52,17.65,18.26,
     18.76,24.06,24.86,25.75,28.2,
     29.62, 29.99)
     #1,1.13,1.5, #23Puntos
     #8.85,9, #25Puntos
     #9.5,9.8, #27Puntos
     #16.6,16.8,17, #30Puntos
     #17.2,17.4,23,23.3,23.6, #35Puntos
     #27.3,27.5,27.86,28, #39Puntos
     #28.03,28.1,28.7, #42Puntos
     #28.9,29.1,#44Puntos
     #29.4,29.7,30)

#PARTE SUPERIOR
y<-c(3,3.7,3.8,3.8, #4Puntos
     4.48,6.65,5.4,7.19, #8Puntos
     6.6,5.4,4.79, #11Puntos
     6.4,7.4,7.2,6,5.85, #16Puntos
     5.9,4.18, #18Puntos
     4.5,3, #20Puntos
     
     #PARTE INFERIOR
     2.99,2.65,
     2.8,3.1,3.4,3,2.5,2,1.45,1.76,1.24,
     1.4,1.5,1.7,1.7,1.4,1.7,1.1,
     1.2,1.27,1.36,1.9,2.99)
     #2.9,2.7,2.5, #23Puntos
     #2.5,2.3,#25Puntos
     #2.5,2.5, #27Puntos
     #2.5,2.5,2.7, #30Puntos
     #2.56,2.5,2.5,2.4,2.5, #35Puntos
     #2.5,2.5,2.7,3, #39Puntos
     #2.67,2.5,2.5, #42Puntos
     #2.38,2.5, #44Puntos
     #2.5,2.5,3)

#Cantidad de puntos seleccionados
#De X
length(x)
#De Y
length(y)

#Función de interpolación

DibujarLinea<-function(inicio, final,yCalculado)
{
  xi0 = x[inicio:final]                               #Determinar que puntuos inciales x voy a usar.
  yi0 = y[inicio:final]                               #Determinar que puntuos inciales y voy a usar.
  x0 <- seq(x[inicio], x[final],len=20)  #Secuencia entre los puntos, con n+1 nodos distintos
  y0 <- barylag(xi0, yi0, x0)                         #Método de Lagrange Baricentrica, para la interpolación
  lines(x0, y0, col="blue")                           #Línealización de la gráfica dado los puntos encontrados
  Ajuste_Polinomio = poly.calc(x0,y0)                 #Cálculo del polinomio característico, con los puntos interpolados
  for(i in inicio:final)
  {
    yCalculado[i]<-Ajuste_Polinomio(y[i])             #Determinar el punto teórico en el polinomio caracteristico
  }
  return(yCalculado)                                  #Devoler arreglo con los "y"  experimentales
}

CalcularError<-function(inicio,final,yCalculado,Error)
{
  
    for(i in inicio:final)
    {
     Error[i]<-(abs(y[i]-yCalculado[i])/y[i])      #Cálcular el error que se genera este valor 
                                                   #experimental, respecto al teórico
    }
  
  return(Error)                                       #Devoler arreglo con los errores
}
CalcularOperaciones<-function(operaciones,inicio, final)
{
  operaciones<-operaciones+final+inicio #El número de operaciones que realizo en la secuencias para calcular los diferentes nodos
  operaciones<-operaciones+final-(inicio+1) #El número de operaciones realizadas en Lagrange Baricentrica.(n+1)nodos distintos
  operaciones<-operaciones+final-inicio #Ajuste polinómico calculado en el valor teórico realizado 
  operaciones<-operaciones+final -inicio # calculo del error en cada nodo.
  return(operaciones)
}
jaccard<-function(y, yCalculado)
{
  aciertos<-0
  desaciertos<-0
  for(i in 1: length(y))
  {
    #if((y[i]==yCalculado[i])|| ((yCalculado[i]>=y[i]+y[i]*0.1)&&(yCalculado[i]<=y[i]-0.1*y[i])))
    if(y[i]==yCalculado[i] || (y[i]-yCalculado[i]<=1&&y[i]-yCalculado[i]>=0)  || (yCalculado[i]-y[i]<=1&&yCalculado[i]-y[i]>=0))
    {
      aciertos<-aciertos+1
    }
    else
    {
      desaciertos<-desaciertos+1
    }
  }
  cat("Número de aciertos: ", aciertos)
  cat("Número de desaciertos: ", desaciertos)
  cat("Índice de Jaccard: ", aciertos/length(y))
}




plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Doggo")

#Parte superior del perro

yCalculado=DibujarLinea(1,4,yCalculado)
Error=CalcularError(1,4,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,1,4)

yCalculado=DibujarLinea(4,8,yCalculado)
Error=CalcularError(4,8,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,4,8)


yCalculado=DibujarLinea(8,11,yCalculado)
Error=CalcularError(8,11,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,8,11)

yCalculado=DibujarLinea(11,16,yCalculado)
Error=CalcularError(11,16,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,11,16)

yCalculado=DibujarLinea(16,18,yCalculado)
Error=CalcularError(16,18,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,16,18)

yCalculado=DibujarLinea(18,20,yCalculado)
Error=CalcularError(18,20,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,18,20)
#Parte inferior del perro

yCalculado=DibujarLinea(21,22,yCalculado)
Error=CalcularError(21,22,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,21,22)

yCalculado=DibujarLinea(22,24,yCalculado)
Error=CalcularError(22,24,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,22,24)

yCalculado=DibujarLinea(24,26,yCalculado)
Error=CalcularError(24,26,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,24,26)

yCalculado=DibujarLinea(26,27,yCalculado)
Error=CalcularError(26,27,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,26,27)

yCalculado=DibujarLinea(27,28,yCalculado)
Error=CalcularError(27,28,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,27,28)

yCalculado=DibujarLinea(28,30,yCalculado)
Error=CalcularError(28,30,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,28,29)

yCalculado=DibujarLinea(30,32,yCalculado)
Error=CalcularError(30,32,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,30,23)

yCalculado=DibujarLinea(32,34,yCalculado)
Error=CalcularError(32,34,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,32,34)

yCalculado=DibujarLinea(34,35,yCalculado)
Error=CalcularError(34,35,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,34,35)

yCalculado=DibujarLinea(35,37,yCalculado)
Error=CalcularError(35,37,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,35,37)

yCalculado=DibujarLinea(37,38,yCalculado)
Error=CalcularError(37,38,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,37,38)

yCalculado=DibujarLinea(38,40,yCalculado)
Error=CalcularError(38,40,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,38,40)

yCalculado=DibujarLinea(40,42,yCalculado)
Error=CalcularError(40,42,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,40,42)

yCalculado=DibujarLinea(42,43,yCalculado)
Error=CalcularError(42,43,yCalculado,Error)
operaciones=CalcularOperaciones(operaciones,42,43)

print(Error)
for(i in 1:43)
{
  ErrorTotal<-Error[i]
}
print(ErrorTotal)
#Impresion de resultados
tabla<-matrix(c(x,y,yCalculado,Error),ncol = (length(x)), byrow = TRUE)
colnames(tabla)<-c("Datos 1","Datos 2","Datos 3","Datos 4","Datos 5","Datos 6","Datos 7","Datos 8","Datos 9","Datos 10",
                   "Datos 11","Datos 12","Datos 13","Datos 14","Datos 15","Datos 16","Datos 17","Datos 18","Datos 19" ,"Datos 20"
                   ,"Datos 21","Datos 22","Datos 23","Datos 24","Datos 25","Datos 26","Datos 27","Datos 28","Datos 29","Datos 30"
                   ,"Datos 31","Datos 32","Datos 33","Datos 34","Datos 35","Datos 36","Datos 37","Datos 38","Datos 39","Datos 40"
                   ,"Datos 41","Datos 42","Datos 43")
rownames(tabla)<-c("X","Y","Y1","Error")
tabla<-as.table(tabla)
tabla
cat("El error relativo total por el número de iteraciones realizada es:",ErrorTotal,"%")
cat("El número de operaciones realizadas en la intrpolación es:",operaciones)
jaccard(y,yCalculado)


