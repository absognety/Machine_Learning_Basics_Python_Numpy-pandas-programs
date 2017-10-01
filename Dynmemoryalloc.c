#include<stdio.h>
#include<conio.h>
#include<stdlib.h>
void main()
{
   int i,n,count=0;
   int *x;
   printf("\nEnter the number n....");
   scanf("%d",&n);
   x=(int *)malloc(n*sizeof(int));
   printf("\nEntering the array elements........");
   for(i=0;i<n;i++)
   {
      scanf("%d",x+i);
   }
   for(i=0;i<n;i++)
   {
      count=count+1;
   }
   printf("\nThe number of elements in the array is %d",count);
}
