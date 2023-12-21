
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
apptainer run image.sif H2_results.R 1151 1200
apptainer run image.sif H2_results.R 1201 1250
apptainer run image.sif H2_results.R 1251 1300
apptainer run image.sif H2_results.R 1301 1350
apptainer run image.sif H2_results.R 1351 1400
apptainer run image.sif H2_results.R 1401 1450
apptainer run image.sif H2_results.R 1451 1500
apptainer run image.sif H2_results.R 1501 1550
apptainer run image.sif H2_results.R 1551 1600
apptainer run image.sif H2_results.R 1601 1650
apptainer run image.sif H2_results.R 1651 1700
apptainer run image.sif H2_results.R 1701 1750
apptainer run image.sif H2_results.R 1751 1800
apptainer run image.sif H2_results.R 1801 1850
apptainer run image.sif H2_results.R 1851 1900
apptainer run image.sif H2_results.R 1901 1950
apptainer run image.sif H2_results.R 1951 2000
apptainer run image.sif H2_results.R 2001 2050
apptainer run image.sif H2_results.R 2051 2100
apptainer run image.sif H2_results.R 2101 2150
apptainer run image.sif H2_results.R 2151 2200
apptainer run image.sif H2_results.R 2201 2250
apptainer run image.sif H2_results.R 2251 2300
apptainer run image.sif H2_results.R 2301 2350
apptainer run image.sif H2_results.R 2351 2400
apptainer run image.sif H2_results.R 2401 2450
apptainer run image.sif H2_results.R 2451 2500
apptainer run image.sif H2_results.R 2501 2550
apptainer run image.sif H2_results.R 2551 2600
apptainer run image.sif H2_results.R 2601 2650
apptainer run image.sif H2_results.R 2651 2700
apptainer run image.sif H2_results.R 2701 2750
apptainer run image.sif H2_results.R 2751 2800
apptainer run image.sif H2_results.R 2801 2850
apptainer run image.sif H2_results.R 2851 2900
apptainer run image.sif H2_results.R 2901 2950
apptainer run image.sif H2_results.R 2951 3000
apptainer run image.sif H2_results.R 3001 3050
apptainer run image.sif H2_results.R 3051 3100
apptainer run image.sif H2_results.R 3101 1187
