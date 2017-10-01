#include<stdio.h>
#include<conio.h>
void main()
{
  struct node
  {
    int data;
    struct node *next;
  };
  struct node *ptr,*head=NULL,*last=NULL;
  struct node *temp;
  int a;
  printf("\nEnter the data to be inserted into the node....");
  scanf("%d",&a);
  ptr->data=a;
  ptr->next=NULL;
  if(head==NULL && last==NULL)
     head=last=ptr;
  else
  {
     ptr->next = head;
     head=ptr;
  }
  printf("%d",ptr->data);
  printf("\n%d",head->data);
}
