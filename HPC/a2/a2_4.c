#include <stdio.h>
#include <math.h>
#include <omp.h>
#include <sys/time.h>

int main(int argc, char const *argv[])
{
    const int N = 1000;
    int i, j;

    double A[N*N];
    double x[N], b[N];

    struct timeval ts;
    struct timeval te;

    int chunk = 5;
    int thread_id;
    
    for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
        A[i*N + j] = sin(0.01*(i*N + j));
      }
      b[i] = cos(0.01*i);
      x[i] = 0.0;
    }
    
    #pragma omp parallel private(i, j, ts, te, thread_id) shared(A, x, b, chunk)
    {
      gettimeofday(&ts, NULL);
      // matrix vector multiplication
      #pragma omp for schedule(static, chunk) nowait
      for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            x[i] += A[i*N + j] * b[j];
        }
      }
      gettimeofday(&te, NULL);
      thread_id = omp_get_thread_num();
      long t_usec = te.tv_usec - ts.tv_usec;
      printf("Thread %d : %ld us\n", thread_id, t_usec);
    }
    
    printf("x[%d] = %g\n", 404, x[404]);
    return 0;
}
