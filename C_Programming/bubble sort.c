#include<stdio.h>
#include<conio.h>
#include<stdlib.h>
void main()
{
  int n;
  int *a,i,j,c,k;
  a=(int *)malloc(n*sizeof(int));
  printf("\nEnter the size of the array.....");
  scanf("%d",&n);
  for(i=0;i<=n-1;i++)
  {
     printf("\nEnter the element of the array.....");
     scanf("%d",a+i);
  }
  for(i=0;i<=n-1;i++)
  {
    printf("\n%d",*(a+i));
  }
  k=n-1;
  for(i=0;i<=n-1;i++)
  {
      for(j=0;j<k;j++)
      {
        if(a[j]>a[j+1])
        {
            c=a[j];
            a[j]=a[j+1];
            a[j+1]=c;
        }
      }
      k--;
  }
  for(i=0;i<=n-1;i++)
    printf("\n%d",a[i]);
}
