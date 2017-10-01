#include<stdio.h>
#include<conio.h>
void main()
{
  int x,y;
  int *ptr1,*ptr2;
  ptr1=&x;
  ptr2=ptr1;
  printf("\n%u",ptr1);
  printf("\n%d",*ptr1);
  printf("\n%u",ptr2);
  printf("\n%d",*ptr2);
}
