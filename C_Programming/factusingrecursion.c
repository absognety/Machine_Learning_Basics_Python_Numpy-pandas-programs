#include<stdio.h>
#include<conio.h>
int factorial(int);
void main()
{
  int n,result;
  printf("\nEnter the factorial of a number to be calculated.....");
  scanf("%d",&n);
  result=factorial(n);
  printf("The factorial of %d is %d",n,result);
}
int factorial(int b)
{
  int fact;
  if(b==0)
     return 1;
  else
  {
     fact=b*factorial(b-1);
     return(fact);
  }
}
