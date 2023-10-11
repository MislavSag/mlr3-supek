#!/bin/bash

#PBS -N H2
#PBS -l ncpus=4
#PBS -l mem=15GB
#PBS -J 1-1933

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_job.R
