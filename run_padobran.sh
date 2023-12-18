#!/bin/bash

#PBS -N H2_PREPARE
#PBS -l mem=64GB

cd ${PBS_O_WORKDIR}
apptainer run image.sif run_padobran.R
