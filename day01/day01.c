#include <stdio.h>

int requiredFuel(int mass)
{
    return mass / 3 - 2;
}

int main()
{
    int mass;
    int total = 0;
    while (scanf("%d", &mass) != EOF)
    {
        total += requiredFuel(mass);
    }
    printf("%d\n", total);
    return 0;
}
