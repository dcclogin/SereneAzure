#!/bin/bash

#SBATCH -J a2_4_c
#SBATCH -p debug
#SBATCH -o a2_4_c%j.txt
#SBATCH -e a2_4_c%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cd17@iu.edu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --mem=11000
#SBATCH --time=00:04:00

cd /N/u/cd17/BigRed3/sa/HPC/a2

export OMP_NUM_THREADS=1
time srun -n 1 -N 1 a2_4

export OMP_NUM_THREADS=2
time srun -n 1 -N 1 a2_4

export OMP_NUM_THREADS=4
time srun -n 1 -N 1 a2_4

export OMP_NUM_THREADS=8
time srun -n 1 -N 1 a2_4

export OMP_NUM_THREADS=16
time srun -n 1 -N 1 a2_4

export OMP_NUM_THREADS=24
time srun -n 1 -N 1 a2_4

