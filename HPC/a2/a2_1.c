#include <stdio.h>
#include <omp.h>

int main() {
    int const N=100;
    int i, k;
    double a[N], b[N];
    double dot_prod = 0.0;

    int thread_id;
    for (i = 0; i < N; i++) {
        a[i] = 3.14;
        b[i] = 6.67;
    }

    #pragma omp parallel private(thread_id)
    {
        thread_id = omp_get_thread_num();
        printf("This thread is: %d\n", thread_id);
        
        #pragma omp for reduction(+:dot_prod)
        for (i = 0; i < N; i++) {
            dot_prod = dot_prod + a[i] * b[i];
        }
    }

    printf("Dot product of the two vectors is %g\n", dot_prod);
    return 0;    
}