#include<stdio.h>
#include<conio.h>
void main()
{
    struct node
    {
        int data;
        struct node *next;
    };
    int a,count,n,nodecount=0;
    struct node *ptr,*head=NULL,*last=NULL;
    struct node *temp,*temp1,*temp2;
    printf("\nEnter the data to be inserted into the node...");
    scanf("%d",&a);
    ptr->data=a;
    ptr->next=NULL;
    if(head==NULL && last==NULL)
        head=last=ptr;
    nodecount++;
    else
    {
        printf("\nEnter the position at which data should be inserted...");
        scanf("%d",&n);
        temp1=head;
        temp2=head;
        count=1;
        while(temp1!=NULL)
        {
            temp1=temp2;
            temp2=temp2->next;
            count++;
            if(count==n)
                break;
            temp1->next=ptr;
            ptr->next=temp2;
        }



    }

}
