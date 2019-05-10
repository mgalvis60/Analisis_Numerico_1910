                    #CONJUNTOS

set TIPOS;                 # in i -> I
set INGREDIENTES;          # in j- > J
set PERIODOS;              # in t -> T
set MAQUINARIA ;           # in m -> M

                    #PARAMETROS
param P    {i in TIPOS};                    #  ($/L)      Precio de venta de un litro de tipo cerverza  i .
param CI   {j in INGREDIENTES};             #  ($/kg)     Costo del ingrediente j. 
param CM   {m in MAQUINARIA};               #  ($)         Costo de la maquinaria m .
param I;                                    #  (%)         Impuesto del gobierno. 
param CAN  {i in TIPOS, j in INGREDIENTES}; #  (kg/L)     Cantidad en kg que necesita la cerveza i del ingrediente j. 
param INV  {i in TIPOS};                    #  (L)          Inventario inicial del tipo de cerveza i. 
param D    {i in TIPOS, j in PERIODOS};     #  (L)          Demanda del tipo de cerveza i en el periodo j, 
param DIS  {j in INGREDIENTES};             #  (kg)         Cantidad disponible del ingrediente j . 
param TM   {i in TIPOS};                    #  (meses)    Tiempo máximo de almacenamineto del producto i .  
param PRO  {i in TIPOS, t in PERIODOS};     #  (meses/L)  fecha de producción en litros del tipo de cerveza i en el periodo t. 
param INVERSION;                            #  ($)            Inversion incial del proyecto . 



                    #VARIABLES
var X    {i in TIPOS, t in PERIODOS} >=0 ; # (L) Cantidad en litros del tipo cerveza i para producir en el periodo t. 
var Y    {i in TIPOS, t in PERIODOS} >=0;  # (L) Cantidad en litros del tipo cerveza i para almacenar en el periodo t.
var INVF {i in TIPOS} >=0;                 # (L) Inventario final del producto i . 
var L    {m in MAQUINARIA}>=0 binary;      #Variable de decisión binaria   1 ->  compro la máquina m. 
                                                                   #2 ->  no compro la máquina m.  

                    #FUNCION OBJETIVO

maximize Z: sum {i in TIPOS,t in PERIODOS } X[i,t]*P[i] - ( sum {i in TIPOS,j in INGREDIENTES, t in PERIODOS} X[i,t] * CAN[i,j] * CI [j] )-( sum{m in PERIODOS } L[m] * CM[m]); #Máximizar las utilidades del negocio 

                    #RESTRICCIONES

# Inventario incial de las cervezas de acuerdo a su tipo. 
s.t. InventarioInicial {i in TIPOS, t in PERIODOS} : INV[i] + X[i,t] - D[i,t] = Y[i,t] ;

# Inventario intermedio  de las cervezas de acuerdo a su tipo. 
s.t. InventarioIntermedio {i in TIPOS, t in PERIODOS} : Y[i,t] + D[i,t]  = Y[i,t+1] ; 

#Inventario  final 
s.t. InventarioFinal {i in TIPOS, t in PERIODOS} : INVF[i] = Y[i,t] ; 

#Máximo tiempo de almacenamiento 
#s.t.  TiempoMaximoAlmacenamiento {i in TIPOS}: sum{t in PERIODOS } Y[i,t] * PRO[i,t] <= TM [i] ;

#Límite materia prima 

s.t. LimiteMateriaPrima {j in INGREDIENTES }: sum {i in TIPOS, t in PERIODOS} X[i,t]* CAN[i,j] <= DIS[j] ; 

#Límite Inversion 
s.t.  LimiteInversion :  ( sum {i in TIPOS, j in INGREDIENTES, t in PERIODOS} X[i,t] * CAN[i,j] * CI [j] ) + (sum{m in PERIODOS } L[m] * CM[m])<= INVERSION; 





solve;
display Z;
display X;
display Y;
#display Unica_Tarea_Por_Individuo;
#display Asignacion_Tareas;



data;

#Tabla 1 (Conjuntos)
set INDIVIDUOS:= P1 P2 P3 P4 P5 P6 P7 P8 P9 P10 P11 P12 P13 P14 P15 P16 P17 P18 P19 P20;
set TAREAS:= A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14 A15 A16 A17 A18 A19 A20;

#Tabla2 (Parámetro de Eficiencia)
param L:  A1  A2  A3  A4  A5  A6  A7  A8 A9 A10 A11 A12 A13 A14 A15 A16 A17 A18 A19 A20:=  
P1        1   4   9   6   7   0   3   9  0   4   4   4   5   4   8   0   8   7   8   7
P2        3   6   7   8   8   2   4   7  2   6   3   8   7   8   9   8   7   9   5   6
P3        2   8   7   0   4   4   7   3  3   8   4   9   10  9   5   5   5   6   4   8
P4        7   5   4   5   3   9   9   2  4   2   10  3   2   6   2   1   3   7   7   5
P5        10  8   3   6   7   5   3   1  7   1   9   2   3   8   4   10  2   4   1   4
P6        8   2   5   3   2   8   2   3  8   3   8   10  6   2   6   8   7   5   4   2
P7        9   3   4   2   8   1   5   3  3   5   5   7   9   4   8   7   9   1   7   5
P8        4   8   2   5   6   2   5   8  7   8   6   7   2   10  3   5   8   2   8   1
P9        7   2   6   10  2   7   1   0  4   1   8   8   3   7   0   8   2   9   0   7
P10       3   4   5   0   9   5   3   2  1   5   3   6   7   9   2   6   4   10  2   4
P11       5   8   6   4   3   0   5   10 2   2   2   7   9   10  8   8   7   6   8   4
P12       4   7   2   8   1   0   3   5  5   2   6   9   9   5   4   2   8   5   2   2
P13       2   4   1   3   2   7   8   3  4   8   6   5   5   9   8   3   7   6   3   1
P14       7   4   6   9   5   4   2   8  6   6   6   8   4   7   9   6   2   10  6   5
P15       3   6   3   4   2   8   9   2  1   6   7   9   10  5   4   9   4   7   7   7
P16       5   8   7   3   2   5   8   2  5   7   9   7   8   9   1   3   2   7   7   9
P17       4   7   9   5   3   2   5   3  8   5   3   8   4   7   8   8   5   7   9   2
P18       5   2   6   4   2   8   8   9  7   6   0   0   3   10  8   7   4   9   2   4
P19       4   8   1   0   5   7   5   3  2   0   8   9   7   8   5   8   4   1   0   3
P20       3   6   7   9   7   4   2   1  9   4   6   8   2   4   2   7   8   9   10  8;   




end data;

