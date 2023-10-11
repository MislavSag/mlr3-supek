#!/bin/bash

#PBS -N H2
#PBS -l select=1:ncpus=4:mem=15GB
#PBS -J 1-1933

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R
