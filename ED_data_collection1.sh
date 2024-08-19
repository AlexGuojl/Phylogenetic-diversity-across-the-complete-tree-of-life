#!/bin/bash
#SBATCH -J ED_estimation
#SBATCH -p cn-long
#SBATCH -N 1
#SBATCH -o ED_estimation%j.out
#SBATCH -e ED_estimation%j.err
#SBATCH --no-requeue
#SBATCH -A jchamper_g1
#SBATCH --qos=jchampercnl
#SBATCH -c 1
pkurun  sleep 1


mkdir -p ed_data_collection

# 
arg_values=(0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0);


for arg_value in "${arg_values[@]}"
do
    python3 ED_driver.py --arg1 "$arg_value" > "ed_data_collection/${arg_value}.part"
done


wait

cd ed_data_collection