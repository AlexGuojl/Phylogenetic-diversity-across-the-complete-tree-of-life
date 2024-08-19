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

# 创建 ed_data_collection 文件夹
mkdir -p ed_data_collection

# 定义一系列 arg_value 值
arg_values=(0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0);

# 遍历每一个 arg_value 并传递给 driver2.py
for arg_value in "${arg_values[@]}"
do
    # 将每次的输出保存到一个唯一的 .part 文件中
    python3 driver3.py --arg1 "$arg_value" > "ed_data_collection/${arg_value}.part"
done

# 等待所有进程结束
wait

cd ed_data_collection