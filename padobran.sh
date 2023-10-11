#!/bin/bash

#PBS -N H2
#PBS -l ncpus=6
#PBS -J 1-1932

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R
