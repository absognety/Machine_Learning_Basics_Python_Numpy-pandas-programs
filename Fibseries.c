#include <stdio.h>
#include <conio.h>
void main()
{
    int i,n;
    int a=1,b=1,c;
    printf("\nEnter the number of terms n...");
    scanf("%d", &n);
    printf("%d\t%d", a,b);
    for(i=0;i<=n;i++)
    {
       c=a+b;
       printf("\t%d",c);
       a=b;
       b=c;
    }
}
