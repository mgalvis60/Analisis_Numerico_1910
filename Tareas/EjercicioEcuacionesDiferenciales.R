#Ejercicio Ecuaciones Diferenciales : Ejercicio masa - resorte . 

#Módelo Teórico 

#FunciÓn de posición 
fx1<-function(t)2/3 *cos(8*t) - 1/6 *sin(8*t)
#FUnción de velocidad
fx2<-function(t) -16/3* sin(8*t) - 4/3 * cos(8*t)
#Función de acceleración 
fx3<-function(t) -128/3 * cos(8*t) + 32/3 * sin(8*t) 


#Deterimar la posición después de 3 s. 
XT1<-fx1(3)
XT1
#Deterimar la posición después de 5 s. 
XT2<-fx1(5)
XT2
#Determinar el tiempo donde la masa pase por la posición de equilibrio

#Encuntre la amplitud, periodo, frecuencia 

#Amplitud la formula es: A = ( x(0)^2 + (masa)^2  )^1/2

AT<- sqrt((2/3)^2 + (1/6)^2 )

#Periodo la formula es: T= 2pi/w
TT<- (2*(pi))/8

#La formula de frecuencia es : f= 1/T
f<- 8/(2*(pi))

#Deterimne  la gráfia mediante interpolación gráfica, de posición vs tiempo
x<-c()
y<-c()
x<-c(seq(0,10,0.5))

for(i in 1:21)
{
  y[i] <- fx1(x[i])
}

plot(x,y,pch=19)







