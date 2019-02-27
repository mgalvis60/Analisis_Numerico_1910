lectura<-function()
{
  
  print("Ingrese el número de filas")
  n<-scan(what=double(),nmax = 1)
  print("Ingrese el número de columnas")
  m<-scan(what=double(),nmax = 1)
  if(m-n>1)
  {
    print("La matriz no tiene las dimensiones correctas")
  }
  else
  {
    main(n,m)
    
  }
}

main<-function(n,m)
{
  datos <- scan (,what=double(),n*m)
  print(datos)
  
  A <- matrix(datos,ncol = m,nrow = n, byrow = T)
  print(A)
  
  Dn<-n*n
  Dm<-m*m
  
  diagonal(A,Dn,Dm)
}

diagonal<-function(A,Dn,Dm)
{
  D<-matrix(nrow=Dn, ncol=Dm, byrow=TRUE)
  contn<-1
  contm<-1
  for(i in 1:Dn)
  {
    for(j in 1:Dm)
    {
      if(i==j)
      {
        D[i,j]=A[contn,contm]
        if(contm==sqrt(Dm))
        {
          contn<-contn+1
          contm<-1
        }
        else
        {
          contm<-contm+1
        }
      }
    }
  }
  Dceros(D,k=0)
}

Dceros<-function(D, k = 0) {
  if (k == 0) {
    D[upper.tri(D, diag = FALSE)] <- 0
    D[lower.tri(D, diag = FALSE)] <- 0
  } else {
    M[col(D) >= row(D) + k + 1] <- 0
  }
  return(D)
}

lectura()
