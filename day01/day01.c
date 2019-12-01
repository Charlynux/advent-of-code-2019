#include <stdio.h>

int requiredFuel(int mass)
{
    return mass / 3 - 2;
}

int requiredFuel2(int moduleMass)
{
    int mass = requiredFuel(moduleMass);
    int totalFuel = 0;
    while (mass > 0)
    {
        totalFuel += mass;
        mass = requiredFuel(mass);
    }
    return totalFuel;
}

int main()
{
    int mass;
    int total = 0;
    int total2 = 0;
    while (scanf("%d", &mass) != EOF)
    {
        total += requiredFuel(mass);
        total2 += requiredFuel2(mass);
    }
    printf("%d\n", total);
    printf("%d\n", total2);
    return 0;
}
