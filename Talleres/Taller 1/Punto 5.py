m = float(input("Ingrese el número de cifras decimales: "))
n = float(input("Ingrese el número: "))
r = int( n * 10**4)
n = float( r * 10**-4)
r = int (n)
#print ( r )
dig = 0
while (r != 0):
    r = int( r / 10)
    #print (r)
    dig += 1
#print (dig)
errRedondeo = 1 * 10**(dig-m)
errRelativo = 1 * 10**(1-m)
print ("Número: ", n ,"\nError de redondeo: ", errRedondeo, "\nError relativo: ", errRelativo)