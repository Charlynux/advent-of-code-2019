#include <stdio.h>

int main()
{
    int values[1024];
    int value;
    int index = 0;
    while (scanf("%d,", &value) != EOF)
    {
        values[index] = value;
        index++;
    }

    values[1] = 12;
    values[2] = 2;
    for (int i = 0; values[i] != 99; i += 4)
    {
        if (values[i] != 1 && values[i] != 2)
        {
            return -1;
        }

        int a = values[values[i + 1]];
        int b = values[values[i + 2]];
        int result;
        if (values[i] == 1)
        {
            result = a + b;
        }
        else if (values[i] == 2)
        {
            result = a * b;
        }
        values[values[i + 3]] = result;
    }

    printf("%d", values[0]);

    return 0;
}
