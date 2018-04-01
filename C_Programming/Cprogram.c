#include<stdio.h>
#include<conio.h>

int main()
{
    int *p;
    int n,i,j,*count;
    printf("\nEnter the length of the array.....");
    scanf("%d", &n);
    p = (int *)malloc(n * sizeof(int));
    count = (int *)malloc(n * sizeof(int));
    for(i=0;i<n;i++)
        *(count+i) = 0;
    printf("\nEntering the array elements......");
    for(i=0;i<=n-1;i++)
        scanf("\n\t%d", p+i);
    for(i=0;i<=n-1;i++)
    {
        for(j=0;j<=n-1;j++)
        {
            if(*(p+i) == *(p+j))
                *(count+i) = *(count+i) + 1;
        }
    }
    for(i=0;i<=n-1;i++)
        printf("\n\t%d", *(count+i));
    for(i=0;i<=n-1;i++)
    {
        if(*(count+i) > n/2)
            printf("\nMajority element is %d", *(p+i));
    }
    return 0;
}
