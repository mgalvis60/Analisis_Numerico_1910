lectura<-function()
{
  
  print("Ingrese el n�mero de filas")
  n<-scan(what=double(),nmax = 1)
  print("Ingrese el n�mero de columnas")
  m<-scan(what=double(),nmax = 1)
  if(m-n>1)
  {
    print("Infinitas soluciones")
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
  
  matriz <- matrix(datos,ncol = m,nrow = n, byrow = TRUE)
  print(matriz)
  op<-0
  conta<-1
  for(k in 1:n)
  {
    val<-matriz[k,k]
    for(C in 1:m)
    {
      matriz[k,C]=matriz[k,C]/val
    }
    op<-op+1
    cat("MATRIZ CON PIVOTE","Iteraci�n:",conta,"\n")
    print(matriz)
    
    for(f in 1:n)
    {
      if(f!=k)
      {
        val2=matriz[f,conta]
        if(val2>0)
        {
          signo<-"positivo"
        }
        if(val2<0)
        {
          signo<-"negativo"
        }
        if(val2==0)
        {
          signo<-"nada"
        }
        
        if(signo!="nada")
        {
          for(c in 1:m)
          {
            if(signo=="positivo"||signo=="negativo")
            {
              matriz[f,c]=matriz[f,c]-val2*matriz[k,c]
              op<-op+1
            }
            
          }
          
        }
      }
      
    }
    cat("MATRIZ con Iteraci�n:",conta,"\n")
    print(matriz)
    conta<-conta+1
  }
  for (f in 1:n) 
  {
    for(c in 1:m)
    {
      matriz[f,c]=round(matriz[f,c],4)
      
    }
  }
  cat("=========================\n")
  cat("=========================\n")
  print("MATRIZ CON ESCAlONADA REDUCIDA con (error de redonde 4 cifras):")
  print(matriz)
  cat("\nEl n�mero de operaciones totales:\t",op,"\n")
  cat("=========================\n")
  cat("=========================\n")
}

lectura()
