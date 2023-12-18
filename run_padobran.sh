#!/bin/bash

#PBS -N H2_PREPARE
#PBS -l mem=64

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_padobran.R
