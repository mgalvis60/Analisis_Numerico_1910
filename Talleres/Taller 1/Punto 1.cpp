#include <iostream>
using namespace std;

int horner(int poly[], int n, int x, int &op)
{
    int resultado = poly[0];

    for (int i=1; i<n; i++)
    {
        resultado = resultado*x + poly[i];
        op++;
    }

    return resultado;
}

int main()
{
    cout<<"POLINOMIO 1"<<endl;
    int op1=0;
    int poly1[] = {2, 0, -3, 3, -4};
    int x1 = -2;
    int n1 = sizeof(poly1)/sizeof(poly1[0]);
    cout << "El resultado del polinomio es: " << horner(poly1, n1, x1, op1)<<endl;
    cout<<"El numero  minimo de operaciones fue de: "<<op1<<endl;
    cout<<"El numero minimo de multiplicaciones fue de "<<op1<<" y el grado del polinomio es de "<<n1-1<<endl;
    cout<<endl<<endl;

    cout<<"POLINOMIO 2"<<endl;
    int op2=0;
    int poly2[] = {7, 6, -6, 0, 3, -4};
    int x2 = 3;
    int n2 = sizeof(poly2)/sizeof(poly2[0]);
    cout << "El resultado del polinomio es: " << horner(poly2, n2, x2, op2)<<endl;
    cout<<"El numero  minimo de operaciones fue de: "<<op2<<endl;
    cout<<"El numero minimo de multiplicaciones fue de "<<op2<<" y el grado del polinomio es de "<<n2-1<<endl;
    cout<<endl<<endl;

    cout<<"POLINOMIO 3"<<endl;
    int op3=0;
    int poly3[] = {-5, 0, 3, 0, 2, -4, 0};
    int x3 = -1;
    int n3 = sizeof(poly3)/sizeof(poly3[0]);
    cout << "El resultado del polinomio es: " << horner(poly3, n3, x3, op3)<<endl;
    cout<<"El numero  minimo de operaciones fue de: "<<op3<<endl;
    cout<<"El numero minimo de multiplicaciones fue de "<<op3<<" y el grado del polinomio es de "<<n3-1<<endl;
    return 0;
}