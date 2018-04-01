#include<stdio.h>
#include<conio.h>
void main()
{
  int i,factorial,n,result;
  printf("\nEnter the factorial of a number to be calculated.....");
  scanf("%d",&n);
  factorial=1;
  for(i=n;i>=1;i--)
  {
     factorial=factorial*i;
     result=factorial;
  }
  printf("\nThe factorial of a number is %d",result);
}
