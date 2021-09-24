#include <stdio.h>
#include <math.h>

int main(int argc, char const *argv[])
{
    const int N = 1000;
    int i, j;

    double A[N*N];
    double x[N], b[N];

    //initialize the matrix and the vector
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            A[i*N + j] = sin(0.01*(i*N + j));
        }
        b[i] = cos(0.01*i);
        x[i] = 0.0;
    }
    // matrix vector multiplication
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            x[i] += A[i*N + j] * b[j];
        }
    }
    printf("x[%d] = %g\n", 404, x[404]);
    return 0;
}
