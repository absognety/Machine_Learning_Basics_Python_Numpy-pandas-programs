#include<stdio.h>
#include<conio.h>
void main()
{
  struct customer
  {
     int age;
     struct customer *next;
  };
  struct customer customer1,customer2,*p1,*p2;
  p1=&customer1;
  p2=&customer2;
  p1->next=p2;
  printf("\nEnter any number...");
  scanf("%d",&(p2->age));
  printf("%d",(p1->next)->age);
}
