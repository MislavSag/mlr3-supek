#!/bin/bash

export APPTAINERENV_LD_LIBRARY_PATH="${CRAY_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH}:\$LD_LIBRARY_PATH:/opt/cray/pe/pals/1.2.3/lib"

apptainer exec \
  --bind /opt \
  --bind /run \
  --bind /usr/lib64 \
  image.sif \
  Rscript -e 'Sys.which("qsub")'
