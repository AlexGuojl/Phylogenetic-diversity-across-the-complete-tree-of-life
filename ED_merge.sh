#!/bin/bash
#SBATCH -J ED_merge
#SBATCH -p cn-long
#SBATCH -N 1
#SBATCH -o ED_merge%j.out
#SBATCH -e ED_merge%j.err
#SBATCH --no-requeue
#SBATCH -A jchamper_g1
#SBATCH --qos=jchampercnl
#SBATCH -c 1
pkurun  sleep 1

cd ed_data_collection
cat *.part > ED.csv #每次都要改！
#rm *.part


cd ..