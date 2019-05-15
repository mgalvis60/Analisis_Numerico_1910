#Ejercicio Ecuaciones Diferenciales : Ejercicio masa - resorte . 

#Módelo Teórico 

#a) Deterimar la posición después de 3 s. 
#b) Deterimar la posición después de 5 s. 
#c) Determinar el tiempo donde la masa pase por la posición de equilibrio
#d) Determinar el tiempo donde la masa alance su máximo.
#e) Encuntre la amplitud, periodo, frecuencia 
#f) Determine la gráfica (Utilice Interpolación )
#g) Solucione el sistema por EDO^2(Utilice la libreria), compare la solución con el método exacto
#f) Realice un ajuste de curva utilizabndo la solución exacta


#Solución:  

#FunciÓn de posición 
fx1<-function(t)2/3 *cos(8*t) - 1/6 *sin(8*t)
#FUnción de velocidad
fx2<-function(t) -16/3* sin(8*t) - 4/3 * cos(8*t)
#Función de acceleración 
fx3<-function(t) -128/3 * cos(8*t) + 32/3 * sin(8*t) 


# a)

XT1<-fx1(3)
XT1

# b) 
XT2<-fx1(5)
XT2
# c)

# d) 

# e) 

#Amplitud la formula es: A = ( x(0)^2 + (masa)^2  )^1/2

AT<- sqrt((2/3)^2 + (1/6)^2 )

#Periodo la formula es: T= 2pi/w
TT<- (2*(pi))/8

#La formula de frecuencia es : f= 1/T
f<- 8/(2*(pi))

#f)
x<-c()
y<-c()
x<-c(seq(0,10,0.5))

for(i in 1:21)
{
  y[i] <- fx1(x[i])
}

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="Tiempo", ylab="Posición", main="Tiempo vs Posición, solución Téorica")

curve(fx1,add=T,from =0,to =10)

#g) 


#h) 

#Ajuste de polinómico
curve(fx1,add=T,from =0,to =10)
