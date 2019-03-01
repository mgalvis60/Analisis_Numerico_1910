lectura<-function()
{
  
  print("Ingrese el número de filas")
  n<-scan(what=double(),nmax = 1)
  print("Ingrese el número de columnas")
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
  
  matriz <- matrix(datos,ncol = m,nrow = n, byrow = T)
  print("Matriz inicial: ")
  print(matriz)
  op<-0
  conta<-1
  error<-0
  for(k in 1:n)
  {
    val<-matriz[k,k]
    for(C in 1:m)
    {
      matriz[k,C]=matriz[k,C]/val
    }
    op<-op+1
    #cat("MATRIZ CON PIVOTE","Iteración:",conta,"\n")
    #print(matriz)
    
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
              if(val2>0)
              {
                #error<-error+(0.15/(val2))
              }
              if(val2<0)
              {
                #error<-error+(0.15/(-1)*(val2))
              }
            }
          }
          
        }
      }
      
      
    }
    #cat("MATRIZ con Iteración:",conta,"\n")
    #print(matriz)
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
  cat("\nEl número de operaciones totales: ",op,"Con un error total acumulado de:",error," porciento","\n")
  cat("=========================\n")
  cat("=========================\n")
}

lectura()

