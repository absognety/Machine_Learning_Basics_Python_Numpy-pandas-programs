#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

int main()
{
    int n,i;
    int r;
    printf("\nEnter a number.....");
    scanf("%d", &n);
    printf("%d", sizeof(int));
    printf("%d", sizeof(float));
    printf("%d", sizeof(char));
    printf("%d", sizeof(double));
    printf("%d", sizeof(long));
    i=0;
    while((n%10)!=0)
    {
        r = n%10;
        n = n/10;
        printf("%d", r);
        i++;
    }
    return 0;
}

