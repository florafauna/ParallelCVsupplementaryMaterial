#!/bin/bash
#SBATCH -J weak_512
#SBATCH -o Rout/2_length_pbdMPI.Rout
#SBATCH -t 00:04:00
#SBATCH --ntasks=512                # Number of MPI ranks
#SBATCH --cpus-per-task=1           # Number of cores per MPI rank 
#SBATCH --nodes=256                 # Number of nodes
#SBATCH --ntasks-per-node=2         # How many tasks on each node


export OMP_THREAD_LIMIT=1
export OMP_NUM_THREADS=1

mkdir -p RData figs Rout

### Run the executable
/usr/lib64/openmpi/bin/mpiexec Rscript --vanilla 2_length_pbdMPI.R
