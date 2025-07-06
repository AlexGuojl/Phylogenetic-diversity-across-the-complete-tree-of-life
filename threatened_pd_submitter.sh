#!/bin/bash
#PBS -l select=1:ncpus=1:mem=100gb
#PBS -l walltime=72:00:00
#PBS -N threatened_PD
#PBS -o threatened_PD.out
#PBS -e threatened_PD.err

# 切换到当前工作目录
cd $PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a


# 设置当前的 arg_value

# 调用 Python 脚本
python3 threatened_pd.py 

wait

