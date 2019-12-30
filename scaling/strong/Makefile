## this makefile should help to execute the files in the right order.

.PHONY: all 0 1 2 3 4 5 6 rm

all: 6 5 4 3 2 1 0

0: 2_scaling_strong.R
	mpiexec  -np 1 Rscript --vanilla 2_scaling_strong.R 0

1: 2_scaling_strong.R
	mpiexec  -np 2 Rscript --vanilla 2_scaling_strong.R 0
	mpiexec  -np 2 Rscript --vanilla 2_scaling_strong.R 0.05
	mpiexec  -np 2 Rscript --vanilla 2_scaling_strong.R 0.1
	mpiexec  -np 2 Rscript --vanilla 2_scaling_strong.R 0.2

2: 2_scaling_strong.R
	mpiexec  -np 4 Rscript --vanilla 2_scaling_strong.R 0
	mpiexec  -np 4 Rscript --vanilla 2_scaling_strong.R 0.05
	mpiexec  -np 4 Rscript --vanilla 2_scaling_strong.R 0.1
	mpiexec  -np 4 Rscript --vanilla 2_scaling_strong.R 0.2

3: 2_scaling_strong.R
	mpiexec  -np 8 Rscript --vanilla 2_scaling_strong.R 0
	mpiexec  -np 8 Rscript --vanilla 2_scaling_strong.R 0.05
	mpiexec  -np 8 Rscript --vanilla 2_scaling_strong.R 0.1
	mpiexec  -np 8 Rscript --vanilla 2_scaling_strong.R 0.2

4: 2_scaling_strong.R
	mpiexec  -np 16 Rscript --vanilla 2_scaling_strong.R 0
	mpiexec  -np 16 Rscript --vanilla 2_scaling_strong.R 0.05
	mpiexec  -np 16 Rscript --vanilla 2_scaling_strong.R 0.1
	mpiexec  -np 16 Rscript --vanilla 2_scaling_strong.R 0.2


5: 2_scaling_strong.R
	mpiexec  -np 32 Rscript --vanilla 2_scaling_strong.R 0
	mpiexec  -np 32 Rscript --vanilla 2_scaling_strong.R 0.05
	mpiexec  -np 32 Rscript --vanilla 2_scaling_strong.R 0.1
	mpiexec  -np 32 Rscript --vanilla 2_scaling_strong.R 0.2

6: 2_scaling_strong.R
	mpiexec  -np 64 Rscript --vanilla 2_scaling_strong.R 0
	mpiexec  -np 64 Rscript --vanilla 2_scaling_strong.R 0.05
	mpiexec  -np 64 Rscript --vanilla 2_scaling_strong.R 0.1
	mpiexec  -np 64 Rscript --vanilla 2_scaling_strong.R 0.2

