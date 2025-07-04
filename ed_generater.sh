#!/bin/bash
#PBS -l select=1:ncpus=1:mem=5gb
#PBS -l walltime=00:10:00
#PBS -N generate_shell

cd $PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a

#cp -r ed_data_collection/\$arg_value* \$TMPDIR/ed_data_collection/
#cd \$TMPDIR/ed_data_collection

for i in {1..1000}; #replace the number here with random_seeds
do
    filename="full${i}.sh"  
    cat << EOF > ${filename}
#!/bin/bash
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -l walltime=50:00:00
#PBS -N ED_estimation_${i}
#PBS -o ED_estimation_${i}.out
#PBS -e ED_estimation_${i}.err


cd \$PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a


arg_value=${i}

python3 ED_driver.py --arg1 "\$arg_value" > "\$arg_value.part"

wait

EOF
done


