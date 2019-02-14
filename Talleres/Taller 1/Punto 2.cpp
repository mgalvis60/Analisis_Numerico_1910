#include <iostream>

using namespace std;

int main()
{
    int n;
    int d;
    cout<<"Ingrese el n: ";
    cin>>n;
    int divisiones=0;
    while (n>0){
        d=n%2;
        n=n/2;
        divisiones++;
    }
    cout<<"El numero de divisiones fue "<<divisiones;
}