dato=read.table("http://verso.mat.uam.es/~joser.berrendero/datos/challenger.txt",header =TRUE)

x <- c(32,64,96,118,126,144,152.5,158)
y <- c(99.5,104.8,108.5,100,86,64,35.3,15)
plot(x,y,pch=19)
#Ajuste a un polinomio de gradp uno:
fit<-lm(y~x)
summary(fit)
fitted.values
#seguno coeficiente
fit2<- lm(y~poly(x,2,raw=TRUE))
summary(fit2)
#Tercer grado
fit3<-lm(y~poly(x,3,raw=TRUE))
summary(fit3)
#cuarto grado
fit4<-lm(y~poly(x,4,raw=TRUE))
summary(fit4)
#Generación de números de 50 numeros
#Página web
#https://rpubs.com/joser/logistica