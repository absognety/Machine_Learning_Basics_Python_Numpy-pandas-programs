#include<stdio.h>

class Box{
int length;
int width;
int height;

int volume(int length, int width,int height){
int volume;
volume = length * width * height;
return volume;
}
};

class Program{
public static void main(string args[])
{
    int v;
    v = Box.volume(2,4,8);
    System.out.println(v);
}
};


