
#!/bin/bash

#PBS -N H2results
#PBS -l ncpus=1
#PBS -l mem=50GB
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif H2_results.R 901 901
apptainer run image.sif H2_results.R 951 953
