#include<stdio.h>
#include<conio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
int main(){
  int *a;
  int i,j,n;
  int *count;
  printf("\nEnter the value of n....");
  scanf("%d", &n);
  count = (int *)malloc(n*sizeof(int));
  for(i=0;i<=n-1;i++)
  {
      *(count+i) = 0;
  }
  a = (int *)malloc(n*sizeof(int));
  printf("\nEnter the array elements.....");
  for(i=0;i<=n-1;i++)
  {
      scanf("\n%d", a+i);
  }
  for(i=0;i<=n-1;i++)
  {
     for(j=0;j<=n-1;j++)
     {
        if(*(a+i)==*(a+j))
              *(count+i) = *(count+i)+1;
     }
  }
  for(i=0;i<=n-1;i++)
  {
      printf("The length array element %d is %d", i,*(count+i));
  }
  for(i=0;i<=n-1;i++)
  {
      if(*(count+i) > n/2)
         printf("Majority elements are %d", *(a+i));
  }
  return 0;
}
