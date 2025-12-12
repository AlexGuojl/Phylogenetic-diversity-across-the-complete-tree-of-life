#!/bin/bash
#PBS -l select=1:ncpus=1:mem=10gb
#PBS -l walltime=00:10:00
#PBS -N generate_shell

cd $PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a

#cp -r ed_data_collection/\$arg_value* \$TMPDIR/ed_data_collection/
#cd \$TMPDIR/ed_data_collection



for i in {1..1000}; 
do

    filename="full${i}.sh"  
    
    cat << EOF > ${filename}
#!/bin/bash
#PBS -l select=1:ncpus=12:mem=50gb
#PBS -l walltime=7:00:00
#PBS -N ED_estimation_${i}
#PBS -o ED_estimation_${i}.out
#PBS -e ED_estimation_${i}.err


cd \$PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a


# set arg_value
arg_value=${i}

# 调用 Python 脚本

/usr/bin/time -v python3 ED_driver_3.0_12core.py  --arg1 "\$arg_value" > "\$arg_value.part" 

wait

EOF
done


