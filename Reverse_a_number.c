#include<stdio.h>
#include<conio.h>
#include<math.h>
#include<stdlib.h>
#include<string.h>

void main()
{
   int n,i,r;
   printf("\nEnter the value of n....");
   scanf("%d", &n);
   i=0;
   while((n%10)!= 0)
   {
      r = n%10;
      n = n/10;
      i++;
      printf("%d", r);
   }
}
