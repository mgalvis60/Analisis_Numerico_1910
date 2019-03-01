#Mateo Galvis López 
#1B
#lectura(){
  A = matrix(c( 4,   -1,    -1,   -1, -2.718281828,
               -1,    4,    -1,   -1, 5
               -(9/10), -1,     4,   -1, 6
               -1,   -1,    -1,   -4, 0 ), nrow=5, byrow=TRUE)
  A
  main(A);
#}



main<-function(X)
{
  matriz <- X;
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
    cat("MATRIZ CON PIVOTE","Iteración:",conta,"\n")
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
    cat("MATRIZ con Iteración:",conta,"\n")
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
  cat("\nEl número de operaciones totales:\t",op,"\n")
  cat("=========================\n")
  cat("=========================\n")
}

#lectura()
