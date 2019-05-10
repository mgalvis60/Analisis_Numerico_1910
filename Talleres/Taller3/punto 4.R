#Punto 4
require(PolynomF)
require(pracma)

x=c(35, 45, 55, 65, 75)
y=c(35, 48, 70, 40, 22)

datx=x[1:3]; daty=y[1:3]
#a

polyAjuste = poly.calc(datx, daty)
polyAjuste

plot(datx,daty, pch=19, cex=1, col="red",asp=1)
curve(polyAjuste, add=T)

#b

barylag(datx, daty, c(35, 45, 55))
