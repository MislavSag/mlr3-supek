
#!/bin/bash

#PBS -N H2results
#PBS -l ncpus=1
#PBS -l mem=60GB
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif H2_results.R 1 30
apptainer run image.sif H2_results.R 31 60
apptainer run image.sif H2_results.R 61 90
apptainer run image.sif H2_results.R 91 120
apptainer run image.sif H2_results.R 121 150
apptainer run image.sif H2_results.R 151 180
apptainer run image.sif H2_results.R 181 210
apptainer run image.sif H2_results.R 211 240
apptainer run image.sif H2_results.R 241 270
apptainer run image.sif H2_results.R 271 300
apptainer run image.sif H2_results.R 301 330
apptainer run image.sif H2_results.R 331 360
apptainer run image.sif H2_results.R 361 390
apptainer run image.sif H2_results.R 391 420
apptainer run image.sif H2_results.R 421 450
apptainer run image.sif H2_results.R 451 480
apptainer run image.sif H2_results.R 481 510
apptainer run image.sif H2_results.R 511 540
apptainer run image.sif H2_results.R 541 570
apptainer run image.sif H2_results.R 571 600
apptainer run image.sif H2_results.R 601 630
apptainer run image.sif H2_results.R 631 660
apptainer run image.sif H2_results.R 661 690
apptainer run image.sif H2_results.R 691 720
apptainer run image.sif H2_results.R 721 750
apptainer run image.sif H2_results.R 751 780
apptainer run image.sif H2_results.R 781 810
apptainer run image.sif H2_results.R 811 840
apptainer run image.sif H2_results.R 841 870
apptainer run image.sif H2_results.R 871 900
apptainer run image.sif H2_results.R 901 930
apptainer run image.sif H2_results.R 931 960
apptainer run image.sif H2_results.R 961 990
apptainer run image.sif H2_results.R 991 1020
apptainer run image.sif H2_results.R 1021 1050
apptainer run image.sif H2_results.R 1051 1080
apptainer run image.sif H2_results.R 1081 1110
apptainer run image.sif H2_results.R 1111 1140
apptainer run image.sif H2_results.R 1141 1170
apptainer run image.sif H2_results.R 1171 1187
