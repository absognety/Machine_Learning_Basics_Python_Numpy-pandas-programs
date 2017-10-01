#include<stdio.h>
#include<conio.h>
#include<stdlib.h>
void quicksort(int*,int,int);
void swap(int*,int*);
void main()
{
  int n,low,high;
  int *a,i;
  a=(int *)malloc(n*sizeof(int));
  printf("\nEnter the size of the array.....");
  scanf("%d",&n);
  low=0;
  high=n-1;
  for(i=0;i<=n-1;i++)
  {
     printf("\nEnter the element of the array.....");
     scanf("%d",a+i);
  }
  for(i=0;i<=n-1;i++)
  {
    printf("\n%d",*(a+i));
  }
  quicksort(a,low,(low+high-1)/2);
  quicksort(a,(low+high+1)/2,high);
  for(i=0;i<=n-1;i++)
    printf("%d",*(a+i));
}
void quicksort(int* a,int low,int high)
{
   int i,j,pivot;
   *(a+pivot)=*(a+0);
   i=low;
   j=high;
   while(i<j)
   {
      while(a[i]<=a[pivot] && i<j)
        i++;
      while(a[j]>a[pivot])
        j--;
      if(i<j)
        swap(a+i,a+j);
   }
   swap(a+j,a+pivot);
}
void swap(int* a,int* b)
{
    int* t;
    *t=*a;
    *a=*b;
    *b=*t;
}
