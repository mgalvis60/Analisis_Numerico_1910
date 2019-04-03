#Varibales 
x<-c()
y<-c()
Error<-c()
yCalculado<-c()



#PARTE SUPERIOR 
x<-c(1,1.8,3.5,5, #4Puntos
     6.3,8.25,7,10, #8Puntos
     12.5, 15.4, 17, #11Puntos
     18.4,20.5,21.4,23,24, #16Puntos
     26,28, #18Puntos
     28.6,30, #20Puntos
     
     #PARTE INFERIOR
     1,1.13,1.5, #23Puntos
     8.85,9, #25Puntos
     9.5,9.8, #27Puntos
     16.6,16.8,17, #30Puntos
     17.2,17.4,23,23.3,23.6, #35Puntos
     27.3,27.5,27.86,28, #39Puntos
     28.03,28.1,28.7, #42Puntos
     28.9,29.1,#44Puntos
     29.4,29.7,30)

#PARTE SUPERIOR
y<-c(3,3.7,3.8,3.8, #4Puntos
     4.48,6.65,5.4,7.19, #8Puntos
     6.6,5.4,4.79, #11Puntos
     6.4,7.4,7.2,6,5.85, #16Puntos
     5.9,4.18, #18Puntos
     4.5,3, #20Puntos
     
     #PARTE INFERIOR
     2.9,2.7,2.5, #23Puntos
     2.5,2.3,#25Puntos
     2.5,2.5, #27Puntos
     2.5,2.5,2.7, #30Puntos
     2.56,2.5,2.5,2.4,2.5, #35Puntos
     2.5,2.5,2.7,3, #39Puntos
     2.67,2.5,2.5, #42Puntos
     2.38,2.5, #44Puntos
     2.5,2.5,3)

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
  x0 <- seq(x[inicio], x[final],len=(length(xi0)+1))  #Secuencia entre los puntos, con n+1 nodos distintos
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
  #Determinar que puntuos inciales y voy a usar
  for(i in inicio:final)
  {
    Error[i]<-(abs(y[i]-yCalculado[i])/y[i])      #Cálcular el error que se genera este valor experimental, respecto al teórico
  }
  return(Error)                                       #Devoler arreglo con los errores
}

#0.1              #teorico   #Verdaderos
jaccard<-function(yCalculado, yAjustado)
{
  aciertos<-0
  desaciertos<-0
  for(i in 1:length(yCalculado))
  {
    x<-0
    for(j in 1:length(yAjustado))
    {
      if(yCalculado[i]==yAjustado[j])
      {
        x<-1
      }
    }
    if(x==1)
    {
      aciertos<-aciertos+1
    }
    else
    {
      desaciertos<-desaciertos+1
    }
  }
  
  cat("Numero de Aciertos: ",aciertos)
  cat("Numero de Desaciertos: ",desaciertos)
}

plot(x,y, pch=19, cex=0.5, col = "red", 
     asp=1,xlab="Eje x", ylab="Eje y", main="Perrito")

#Parte superior del perro

yCalculado=DibujarLinea(1,4,yCalculado)

Error=CalcularError(1,4,yCalculado,Error)

yCalculado=DibujarLinea(4,8,yCalculado)
Error=CalcularError(4,8,yCalculado,Error)

yCalculado=DibujarLinea(8,11,yCalculado)
Error=CalcularError(8,11,yCalculado,Error)

yCalculado=DibujarLinea(11,16,yCalculado)
Error=CalcularError(11,16,yCalculado,Error)

yCalculado=DibujarLinea(16,18,yCalculado)
Error=CalcularError(16,18,yCalculado,Error)

yCalculado=DibujarLinea(18,20,yCalculado)
Error=CalcularError(18,20,yCalculado,Error)

#Parte inferior del perro

yCalculado=DibujarLinea(21,23,yCalculado)
Error=CalcularError(21,23,yCalculado,Error)

yCalculado=DibujarLinea(23,24,yCalculado)
Error=CalcularError(23,24,yCalculado,Error)

yCalculado=DibujarLinea(24,25,yCalculado)
Error=CalcularError(25,25,yCalculado,Error)

yCalculado=DibujarLinea(25,27,yCalculado)
Error=CalcularError(27,28,yCalculado,Error)

yCalculado=DibujarLinea(27,28,yCalculado)
Error=CalcularError(27,28,yCalculado,Error)

yCalculado=DibujarLinea(28,30,yCalculado)
Error=CalcularError(28,30,yCalculado,Error)

yCalculado=DibujarLinea(30,32,yCalculado)
Error=CalcularError(30,32,yCalculado,Error)

yCalculado=DibujarLinea(32,33,yCalculado)
Error=CalcularError(32,33,yCalculado,Error)

yCalculado=DibujarLinea(33,34,yCalculado)
Error=CalcularError(33,34,yCalculado,Error)

yCalculado=DibujarLinea(34,35,yCalculado)
Error=CalcularError(34,35,yCalculado,Error)

yCalculado=DibujarLinea(35,36,yCalculado)
Error=CalcularError(35,36,yCalculado,Error)

yCalculado=DibujarLinea(36,39,yCalculado)
Error=CalcularError(36,39,yCalculado,Error)

yCalculado=DibujarLinea(39,41,yCalculado)
Error=CalcularError(39,41,yCalculado,Error)

yCalculado=DibujarLinea(41,42,yCalculado)
Error=CalcularError(41,42,yCalculado,Error)

yCalculado=DibujarLinea(42,43,yCalculado)
Error=CalcularError(42,43,yCalculado,Error)

yCalculado=DibujarLinea(43,44,yCalculado)
Error=CalcularError(43,44,yCalculado,Error)

yCalculado=DibujarLinea(44,47,yCalculado)
Error=CalcularError(44,47,yCalculado,Error)

#Impresion de resultados
tabla<-matrix(c(x,y,yCalculado,Error),ncol = (length(x)), byrow = TRUE)
colnames(tabla)<-c("Datos 1","Datos 2","Datos 3","Datos 4","Datos 5",
                   "Datos 6","Datos 7","Datos 8","Datos 9","Datos 10",
                   "Datos 11","Datos 12","Datos 13","Datos 14","Datos 15",
                   "Datos 16","Datos 17","Datos 18","Datos 19" ,"Datos 20",
                   "Datos 21","Datos 22","Datos 23","Datos 24","Datos 25",
                   "Datos 26","Datos 27","Datos 28","Datos 29","Datos 30",
                   "Datos 31","Datos 32","Datos 33","Datos 34","Datos 35",
                   "Datos 36","Datos 37","Datos 38","Datos 39","Datos 40",
                   "Datos 41","Datos 42","Datos 43","Datos 44","Datos 45",
                   "Datos 46","Datos 47")
rownames(tabla)<-c("X","Y","Y1","Error")
tabla<-as.table(tabla)
tabla

jaccard(yCalculado, y)

for(i in 1:length(yCalculado))
{
  cat("Valor ",i,": ",yCalculado[i])
}