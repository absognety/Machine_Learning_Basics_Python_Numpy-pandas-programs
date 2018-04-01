#include<stdio.h>
#include<conio.h>
void main()
{
   int x,y,z,m;
   int a[3],i;
   printf("\nEnter the numbers x,y and z");
   scanf("%d%d%d",&x,&y,&z);
   for(i=0;i<3;i++)
   {
      a[i]=x;
      x=y;
      y=z;
   }
   for(i=0;i<3;i++)
   {
     printf("%d",a[i]);
   }
   m=a[0];
   for(i=1;i<3;i++)
   {
     if(m<a[i])
        m=a[i];
   }
   printf("The maximum element is %d",m);
}
