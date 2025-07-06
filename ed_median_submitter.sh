#!/bin/bash
#PBS -l select=1:ncpus=1:mem=100gb
#PBS -l walltime=06:00:00
#PBS -N ED_median
#PBS -o ED_median.out
#PBS -e ED_median.err

# 切换到当前工作目录
cd $PBS_O_WORKDIR

module load tools/prod
module load SciPy-bundle/2022.05-foss-2022a


# 设置当前的 arg_value

# 调用 Python 脚本
python3 ed_median.py 


wait