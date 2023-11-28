#!/bin/bash

#PBS -N H2results
#PBS -l ncpus=1
#PBS -l mem=25GB
#PBS -j oe

cd ${PBS_O_WORKDIR}
apptainer run image.sif H2_results.R
