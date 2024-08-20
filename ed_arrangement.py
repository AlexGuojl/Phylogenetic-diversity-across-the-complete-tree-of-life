# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import os
import random
from random import choice
import time
from datetime import datetime

import sys

from tqdm import tqdm

tqdm.pandas(desc='apply')

os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")##change the path to where your required files exist


#read in leaves table and keep necessary columns.


df_leaves = pd.read_csv("updated_ordered_leaves_2.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])
leaves2 = leaves1
leaves2['id'] = leaves2['id'].astype(str)
leaves2 = leaves2.groupby("parent")["id"].apply(lambda x:x.str.cat(sep = ",")).reset_index()


os.chdir("/Users/alexgjl/Desktop/try_ed_estimation_in_HPC/ed_data_collection")#change the path to where the ED.csv exist
df_ed =  pd.read_csv("ED.csv",low_memory=False)
df_ed.loc[-1] = df_ed.columns 
#remove '[' and ']'
df_ed.iloc[:, 0] = df_ed.iloc[:, 0].str.replace('[', '', regex=False)
df_ed.iloc[:, -1] = df_ed.iloc[:, -1].str.replace(']', '', regex=False)


df_ed = df_ed.T

new_columns = [str(i) for i in range(1, len(df_ed.columns) + 1)]
df_ed.columns = new_columns
new_index = [int(i) for i in range(0, len(df_ed.index))]
df_ed.index = new_index

ed_with_id = pd.concat([df_ed, leaves2], axis=1)
ed_with_id = ed_with_id.drop(columns=['id'])

ed_values_all = pd.merge(leaves1,ed_with_id,how = "left",on = "parent")





