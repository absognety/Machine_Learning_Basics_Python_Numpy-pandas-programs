#include<stdio.h>
#include<conio.h>
#include<string.h>
#include<math.h>
#include<stdlib.h>
int main()
{
  int *a;
  int x;
  int i,j,n;
  printf("\nEnter the value of n....");
  scanf("%d", &n);
  a = (int *)malloc(n*sizeof(int));
  printf("\nEnter the sum value....");
  scanf("%d", &x);
  printf("\nEnter the array elements....");
  for(i=0;i<=n-1;i++)
  {
      scanf("\n%d", a+i);
  }
  for(i=0;i<=n-1;i++)
  {
     for(j=i+1;j<=n-1;j++)
     {
         if((*(a+i))+(*(a+j)) == x)
         {
            printf("\n%d%d", *(a+i),*(a+j));
         }
     }
  }
  return 0;
}
