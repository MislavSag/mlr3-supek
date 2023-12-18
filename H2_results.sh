
#!/bin/bash

#PBS -N H2results
#PBS -l ncpus=1
#PBS -l mem=50GB
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif H2_results.R 1 50
apptainer run image.sif H2_results.R 51 100
apptainer run image.sif H2_results.R 101 150
apptainer run image.sif H2_results.R 151 200
apptainer run image.sif H2_results.R 201 250
apptainer run image.sif H2_results.R 251 300
apptainer run image.sif H2_results.R 301 350
apptainer run image.sif H2_results.R 351 400
apptainer run image.sif H2_results.R 401 450
apptainer run image.sif H2_results.R 451 500
apptainer run image.sif H2_results.R 501 550
apptainer run image.sif H2_results.R 551 600
apptainer run image.sif H2_results.R 601 650
apptainer run image.sif H2_results.R 651 700
apptainer run image.sif H2_results.R 701 750
apptainer run image.sif H2_results.R 751 800
apptainer run image.sif H2_results.R 801 850
apptainer run image.sif H2_results.R 851 900
apptainer run image.sif H2_results.R 901 950
apptainer run image.sif H2_results.R 951 1000
apptainer run image.sif H2_results.R 1001 1050
apptainer run image.sif H2_results.R 1051 1100
apptainer run image.sif H2_results.R 1101 1150
apptainer run image.sif H2_results.R 1151 1187
