#include <stdio.h>
#include <stdlib.h>
#include "mmio.h"
#include <math.h>
#include <time.h>

#define PI 3.1415926
#define LOOP 100000000

int main(int argc, char *argv[])
{
    int ret_code;
    MM_typecode matcode;
    FILE *f;
    int M, N, nz;   
    int idx_i, idx_j;
    double L1_norm;

    //This is where we store our 2D matrix.
    //We need the size of the matrix, which we will read from the file and
    //use it to allocate the matrix.
    char *buffer;
    //NOTE: Here we just allocate the whole matrix (that is actually like a 2D array)
    //we will not store it as a sparse matrix. In later assignments and the project,
    //storing in sparse farmats will be required. This simplicity will help serve
    //this assginment.
    double *Matrix_A;

    //Our vectors: vector 'v' is the sine vector we will populate and
    //'x' is the resultant vector from x = A.v 
    double *vector_v, *vector_x;

    if (argc < 2)
	{
		fprintf(stderr, "Usage: %s [martix-market-filename]\n", argv[0]);
		exit(1);
	}
    else    
    { 
        if ((f = fopen(argv[1], "r")) == NULL) 
            exit(1);
    }

    if (mm_read_banner(f, &matcode) != 0)
    {
        printf("Could not process Matrix Market banner.\n");
        exit(1);
    }


    /*  This is how one can screen matrix types if their application */
    /*  only supports a subset of the Matrix Market data types.      */

    if (mm_is_complex(matcode) && mm_is_matrix(matcode) && 
            mm_is_sparse(matcode) )
    {
        printf("Sorry, this application does not support ");
        printf("Market Market type: [%s]\n", mm_typecode_to_str(matcode));
        exit(1);
    }

    if (mm_is_symmetric(matcode)){
        printf("YES IT IS SYMEMTRIC! Becareful with loading. Explicitly replicate values\n");
    }

    /* find out size of sparse matrix .... */

    if ((ret_code = mm_read_mtx_crd_size(f, &M, &N, &nz)) !=0)
        exit(1);


    /* reserve memory for matrix and vectors */
    buffer = (char *) malloc((M * N) * sizeof(double));
    vector_v = (double *) malloc(N * sizeof(double));
    vector_x = (double *) malloc(N * sizeof(double));
    Matrix_A = (double*) buffer;

    //Initialize the Matrix A with all zeros    
    for (idx_i = 0; idx_i < M; idx_i++)
    {
        for(idx_j = 0; idx_j < N; idx_j++)
        {
            //NOTE: The matrix is actually a 1D array
            Matrix_A[(idx_i * M) + idx_j] = 0.0;
        }
    }
   
    //Read the non-zeroes from file and then set them in Matrix A
    int i;
    for (i=0; i<nz; i++)
    {
        //NOTE: For assignment 1 problem dataset there is no values field in the file
        //it means its a value of '1'
	    fscanf(f, "%d %d\n", &idx_i, &idx_j);
        idx_i--;  /* adjust from 1-based to 0-based */
        idx_j--;
        Matrix_A[(idx_i * M) + idx_j] = 1.0;
        
        //check if symmetric then replicate the values on the other side of
        //the diagonal
        if (mm_is_symmetric(matcode)){
            Matrix_A[(idx_j * M) + idx_i] = 1.0;
        }
    }

    if (f !=stdin) fclose(f);

    //Populate the vector v and set vector x to zeroes
    for (i = 0; i < N; i++)
    {
        // populate v
        vector_v[i] = sin(2*PI*i/(N-1));
    }
    for (i = 0; i < M; i++)
    {
        vector_x[i] = 0.0;
    }

    //Perform the Matrix-Vector Multiplication
    //x = A.v
    for (idx_i = 0; idx_i < M; idx_i++)
    {
        for (idx_j = 0; idx_j < N; idx_j++)
        {
            vector_x[idx_i] += vector_v[idx_j] * Matrix_A[(idx_i * M) + idx_j];
        }
    }

    //Calculate the L1 norm of vector_x
    //This is the sum of the absolutes of all elements of the vector x
    L1_norm = 0.0;
    printf("Showing vector_x ...\n");
    printf("index\tvalue\n");
    for (i = 0; i < M; i++)
    {
        printf("%d\t%lg\n", i, vector_x[i]);
        L1_norm += vector_x[i];
    }
    printf("L1_norm of A.v = %lg\n", L1_norm);
    
    // find FLOPS
    float a = 1.1;
    float b = 1.2;
    clock_t t0 = clock();
    for (i = 0; i < LOOP; i++) {
        // empty loop as a reference
    }
    clock_t t1 = clock();
    for (i = 0; i < LOOP; i++) {
        a += b; // addition op
        b *= a; // multiplication op
    }
    clock_t t2 = clock();
    double t_ref = difftime(t1, t0) / CLOCKS_PER_SEC;
    double t = difftime(t2, t1) / CLOCKS_PER_SEC;
    unsigned long flops = (2 * LOOP)/(t - t_ref);
    printf("FLOPS estimated: %lu\n", flops);
	return 0;
}