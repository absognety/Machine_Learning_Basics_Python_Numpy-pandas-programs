#include<stdio.h>
#include<conio.h>
void main()
{
  struct student
  {
    int num;
    struct student *next;
  };
  struct student *ptr,stud1;
  ptr=&stud1;
  int a;
  printf("\nEnter the roll number of the student.....");
  scanf("%d",&a);
  ptr->num=a;
  printf("\nThe roll number of the student is %d",ptr->num);
  printf("\nThe roll number of the student is %d",(*ptr).num);
}
