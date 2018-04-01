#include<stdio.h>
#include<conio.h>
void main()
{
    int g,*p;
    g=5;
    p=&g;
    printf("\n%u",p);
    printf("\n%d",g);
    printf("\n%d",*p);
}
