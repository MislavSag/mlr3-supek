

sh_file = "
#!/bin/bash

#PBS -N H2results
#PBS -l ncpus=1
#PBS -l mem=50GB
#PBS -j oe

cd ${PBS_O_WORKDIR}
"

id_seq = seq(1, 3120, 50)
id_seq_sh = paste0(
  "apptainer run image.sif H2_results.R ",
  id_seq, " ",
  c(id_seq[2:length(id_seq)] - 1, 1187),
  collapse = "\n"
)

sh_file_f = paste0(sh_file, "\n", id_seq_sh)

sh_file_name = "H2_results.sh"
file.create(sh_file_name)
writeLines(sh_file_f, sh_file_name)
