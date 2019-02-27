library(Matrix)
library(pracma)
require(Matrix)
require(pracma)
#Aplicar el método de A=L+D+U

# Punto A

# Dada la siguiente matriz A
A = matrix(c(6, 2, 9, -2,
             9, 3, 12, 9,
             8, 1, 3,  8,
             2, 1, 4, -1), nrow=4, byrow=TRUE)
A

ludec = lu(A)
#Determino L
L <- lu(A) 
L
#Determino D
D <- diag(diag(A))
D
#Determino U
U <-ludec
U
#Aplico el método de Jacobi
A = L %*% U

#Punto B 

A = matrix(c(6, 2, 9, -2,
             9, 3, 12, 9,
             8, 1, 3,  8,
             2, 1, 4, -1), nrow=4, byrow=TRUE)

b <- matrix(c(1.45,3,5.12,4.0), nrow = 4, ncol = 1, byrow = TRUE)

cat("Mediante método de Gauss-Seidel\n")

itersolve(A, b, tol = 1e-9, method = "Gauss-Seidel")

#Punto c 
b<-c(8,15,1,-4) 
f1<-function(A,x,B,n){
  sup<-matrix(0,n,n)
  inf<-matrix(0,n,n)
  diag<-matrix(0,n,n)
  for (i in 0:n){
    for (j in 0:n){
      if(j>i){
        sup[i,j]=A[i,j]
      }
      if(j<i){
        inf[i,j]=A[i,j]
      }
      if(j==i){
        diag[i,j]=A[i,j]
      }
    }
  }
  print(" descomposicion LDU ")
  print("diagonal superior")
  print("  ")
  print(sup)
  print("  ")
  print("diagonal inferior")
  print("  ")
  print(inf)
  print("  ")
  print("diagonal ")
  print("  ")
  print(diag)
  f2(sup,inf,diag,x,B)
  
}
f2<-function(sup,inf,diag,x,B){
  n=0
  print("iteracion ")
  print(n)
  inv=solve(diag)
  err=1
  while( n<5){
    print("iteraci?n ")
    print(n)
    x1<-x
    x<-(inv*(B-(inf+sup)*x1))
    print(x)
    err=(x[1]-x1[1])/x[1]
    print("error: ")
    print(err)
    n=n+1
  }
  
  print("prueba final: ")
  print((inf+sup+diag)*x)
}
x<-c(1,2,3,1)
f1(A,x,b,4)

