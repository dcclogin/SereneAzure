#!/bin/bash

#SBATCH -J a1_6_c
#SBATCH -p general
#SBATCH -o a1_6_c%j.txt
#SBATCH -e a1_6_c%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cd17@iu.edu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:05:00

cd /N/u/cd17/BigRed3/sa/HPC/a1/src

make run
