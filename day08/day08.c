#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    int digit;
    int digits[20000];
    int count = 0;
    while (scanf("%1d", &digit) != EOF)
    {
        digits[count] = digit;
        count++;
    }

    int width = atoi(argv[1]);
    int height = atoi(argv[2]);
    int layerSize = width * height;
    int layers = count / layerSize;
    for (int j = 0; j < height; j++)
    {
        for (int i = 0; i < width; i++)
        {
            int pixel;
            for (int z = 0; (pixel = digits[i + (width * j) + (layerSize * z)]) == 2; z++)
                ;
            printf("%s", pixel ? "X" : " ");
        }
        printf("\n");
    }

    return 0;
}
