#include<stdio.h>
#include<conio.h>
void main()
{
  char c[100];
  int i=0,l=0;
  printf("\nEnter a string....");
  scanf("\n%s",c);
  while(c[i]!='\0')
  {
     l++;
     i++;
  }
  printf("\nLength of the string is %d",l);
}
