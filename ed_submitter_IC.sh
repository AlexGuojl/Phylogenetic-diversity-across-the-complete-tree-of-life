#!/bin/bash
#PBS -l select=1:ncpus=1:mem=1gb
#PBS -l walltime=00:03:00
#PBS -N generate_shell

cd $PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a


cd /rds/general/user/jg621/home/ed_bootstrap


for file in *.sh; do
    if [[ "$file" != "ed_submitter_IC.sh" ]]; then
        qsub "$file"
    fi
done



