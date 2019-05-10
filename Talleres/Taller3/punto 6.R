#Punto 6
library(pracma)

f1<-function(x) (exp(x))
f2<-function(x) (1/(x))

#a
r1<-c(taylor(f1, 0, 4))
r2<-c(taylor(f2, 0, 4))

x<-c(0,1,2,3,4)

for(i in 0: 4)
{
  r1[i]=round(r1[i], 5)
  r2[i]=round(r2[i], 5)
}

datx=x[1:5]
daty1=r1[1:5]
daty2=r2[1:5]

#b
polyAjuste1=poly.calc(datx, daty1)
polyAjuste2=poly.calc(datx, daty2)

polyAjuste1
polyAjuste2

#c
#Se considera que el polinomio es un buen interpolador
#debido a que siempre arrojará los datos que se necesiten
#de una función para todos los casos facilitando la 
#interpolación de estos. No obstante este es el
#método menos eficiente para interpolar teniendo por 
#encima suyo a lagrange, lagrange baricéntrica y
#y diferencias divididas.
