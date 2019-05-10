#Taller 3 

#Punto 2

x=c(100,200,300,400,500,600) #Temperatura(K)
y=c(-160,-35,-4.2,9.0,16.9,21.3) #Moles por centimetro(cm^3)

#Punto a 

#Determinación del polinomio interpolante por el método de Lagrange

DatosX<-x[1:5];DatosY<-y[1:5]
Ajuste_Polinomio_Lagrange = poly.calc(DatosX,DatosY)
Ajuste_Polinomio_Lagrange

#Punto  b
#Calcular el coeficiente virial de segundo(B) y tercer grado(C) a un temperatura de 450 K, por el método de Lagrange.
B1<- Ajuste_Polinomio_Lagrange(450)
cat("El segundo coeficiente virial es a un temperatura de 450k:", B1)
#Para encontar C, el tercer coeficiente virial  se adjunta en el documento el procedimiento
#Se utiliza la formúla de : PV/RT = 1+BV  y  PV/RT = C/V^2. Con el volumen molar del nitrogeno. 
C1<- 2.0791*10^-4
cat("El tercer coeficiente virial es a un temperatura de 450k:", C1)

#Punto C

#Gráfica del polinómio interpolado de la Lagrange Baricentrica

plot(x,y, pch=19, cex=1, col = "orange", asp=1,xlab="Temperatura (Kelvin)", ylab="B(cm^3/mol)", main="Comportamiento del gas no ideal Virial: Nitrogeno")
curve(Ajuste_Polinomio_Lagrange,add=T,from =90,to =800)

#Punto d

#Método de baricéntrica de Lagrange, para calcular puntos intermedios 

Ajuste_Lagrange_Baricentrica<-barylag(x[c(1:6)], y[c(1:6)],c(150,250,350,450,550))
Ajuste_Lagrange_Baricentrica
x1<-c(150,250,350,450,550)
y1<-c(Ajuste_Lagrange_Baricentrica)
y1
#Punto E

#Gráfica del polinómio interpolado por la Lagrange Baricentrica

plot(x1,y1, pch=19, cex=1, col = "red", asp=1,xlab="Temperatura (Kelvin)", ylab="B(cm^3/mol)", main="Comportamiento del gas no ideal: Virial Nitrogeno")
Ajuste_Polinomio_Lagrange_Baricentrica = poly.calc(x1,y1)
curve(Ajuste_Polinomio_Lagrange_Baricentrica,add=T,from =90,to =600)

#Punto F

#Calcular el coeficiente virial de segundo(B) y tercer grado(C) a un temperatura de 450 K, por el método de Lagrange baricentrica. 
B2<-barylag(x[c(1:6)], y[c(1:6)],c(450))
cat("El segundo coeficiente virial es a un temperatura de 450k:", B2)

#Para encontar C, el tercer coeficiente virial  se adjunta en el documento el procedimiento
#Se utiliza la formúla de : PV/RT = 1+BV  y  PV/RT = C/V^2. Con el volumen molar del nitrogeno que es 1.354x10^-5
C2<- 1.8889*10^-4
cat("El tercer coeficiente virial es a un temperatura de 450k:", C2)

#Punto G

#Calcular que método es el mejor para determinar la serie de truncamiento

#Para el método de Lagrange

#Valor téorico del segundo coeficente de viral del nitrogeno = 0.0394 litros; 39.4 cm^3
#Error para el Lagrange
BT<-39.4
#Serie truncada teótica
ST<-(1+BT/1.354*10^-5)
#Precisión serie truncada Lagrange
ST1<-(1+B1/1.354*10^-5)
Error1<-(ST-ST1)/ST*100
#Precision serie truncada Lagrange Baricentrica
ST2<-(1+B2/1.354*10^-5)
Error2<-(ST-ST2)/ST*100
cat("La presición del método de Lagrange para determinar el segundo coeficiente viral es:",Error1)
cat("La presición del método de Lagrange Baricentrica para determinar el segundo coeficiente viral es:",Error2)






