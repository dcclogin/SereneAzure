#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include <time.h>

int main(int argc, char const *argv[])
{
    const int N = 1000;
    int x[N], i, max_x, min_x, sum, sum2;
    float mean, mean2, var;
    sum = 0;
    sum2 = 0;
    max_x = 0;
    min_x = 100;

    #pragma omp parallel for
    for (i = 0; i < N; i++) {
        x[i] = i;
    }
    #pragma omp parallel private(i) shared(x)
    {
        #pragma omp sections
        {
            {
                for (i = 0; i < N; i++) {
                    if (x[i] > max_x) {
                        max_x = x[i];
                    }
                    if (x[i] < min_x) {
                        min_x = x[i];
                    }
                }
                printf("The max of x = %d\n", max_x);
                printf("The min of x = %d\n", min_x);
            }
            #pragma omp section
            {
                for (i = 0; i < N; i++){
                    sum = sum + x[i];
                }
                mean = sum/N;
                printf("Mean of x = %f\n", mean);
            }
            #pragma omp section
            {
                for (i = 0; i < N; i++) {
                    sum2 = sum2 + (x[i] * x[i]);
                }
                mean2 = sum2/N;
            }
        }
    }

    var = mean2 - mean*mean;
    printf("Variance of x = %f\n", var);
    return 0;
}
