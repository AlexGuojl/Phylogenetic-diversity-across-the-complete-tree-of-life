#estimate proportion of dated nodes among these selected clades

import pandas as pd
import numpy as np
import os
import random
from random import choice
import time
from datetime import datetime

from tqdm import tqdm
tqdm.pandas(desc='apply')

os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")##change this to the path that the files existed in your computer

df_leaves = pd.read_csv("updated_ordered_leaves_2.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])
df_nodes = pd.read_csv("updated_ordered_nodes_2.0.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])
nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)


nodes_no_age = pd.DataFrame(nodes,columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0"])
ages = pd.read_csv("latest_node_dates(real_parent_only).csv", low_memory=False)



ages_selected = ages 
#choose one if a node has several date estimates
ls_age  = []
for row in ages_selected.itertuples():
    age = getattr(row,"ages")#can be 1 or a list of node date estimates
    if  ","  in age:#a list of dates
        ls_ages = age.split(",") #form a list of several ages
        age_selected = choice(ls_ages)#randomly select one from several ages
        ls_age.append(age_selected)
    else:#this selected node have only 1 date
        ls_age.append(age)

ages_selected['age'] = ls_age#add a column called age
ages_selected["age"] = pd.to_numeric(ages_selected["age"])
    
ages_selected = pd.DataFrame(ages_selected,columns = ['id', 'age'])
    
ages_bootstrap_final = pd.merge(nodes_no_age, ages_selected, how = "left",on = "id")
ages_bootstrap_final = ages_bootstrap_final.fillna(0)
ages_bootstrap_final.iat[0,6] = 4025.0#give age to the root/node1, always here
ages_bootstrap_final["age"] = pd.to_numeric(ages_bootstrap_final["age"])
getname = pd.DataFrame(df_nodes, columns = ["id","name","real_parent"])

ages_bootstrap_final = pd.merge(ages_bootstrap_final,getname,how = "left",on = "id")


ls_realp_leaf = list(df_leaves["real_parent"])
ls_realp_node = list(df_nodes["real_parent"])
ls_realp = list(set(ls_realp_leaf+ls_realp_node))


def find_age(a):#find node age
    if a == -1:
        return(0)
    else:
        return(ages_bootstrap_final.iloc[a-1,6])

def find_proportion_dated(leaf_id):##return a list of date
    #based on a node id(a)
    list_try = []#list of parents
    list_try.append(leaves1.iat[int(leaf_id)-1,1])
    
    cp = df_nodes.iat[int(list_try[0])-1,2]
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
    #list_try is a list of parents
    list_try = sorted(list_try,reverse = True)
    #print(list_try)
    #list_try = sorted(list(set(ls_realp_node) & set(list_try)),reverse = True)#list of real parent
    list_date = [find_age(i) for i in list_try]
    
    ls_parents_with_age = [i for i in list_date if i > 0]
    return(len(ls_parents_with_age)/len(list_try))


def find_proportion_realparent(leaf_id):##return a list of date
    #based on a node id(a)
    list_try = []#list of parents
    list_try.append(leaves1.iat[int(leaf_id)-1,1])
    
    cp = df_nodes.iat[int(list_try[0])-1,2]
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
    #list_try is a list of parents
    list_try = sorted(list_try,reverse = True)
    #print(list_try)
    #list_try = sorted(list(set(ls_realp_node) & set(list_try)),reverse = True)#list of real parent
    ls_realparents = list(set(ls_realp) & set(list_try))#this is a list include only real_parent for this species

    return(len(ls_realparents)/len(list_try))


#make a node table include only real parent

df_realp = nodes[nodes["id"].isin(ls_realp)]


def find_proportion_dated_nodes(node_id):#return a proportion
    if node_id == 1:
        ls_values = [1,1,1]
        return(ls_values)
    else:
        leaf_lft = nodes1.iat[int(node_id)-1,5]
        leaf_rgt = nodes1.iat[int(node_id)-1,6]
        list_try = nodes1[(nodes1['leaf_lft'] > leaf_lft-1) & (nodes1['leaf_rgt'] < 1+leaf_rgt)]['id'].tolist()
        list_try = sorted(list_try,reverse = True)
    
        list_date = [find_age(i) for i in list_try]
        
        ls_parents_with_age = [i for i in list_date if i > 0]
        ls_realparents = list(set(ls_realp) & set(list_try))
        ls_values = []
        ls_values.append(len(ls_parents_with_age))
        ls_values.append(len(ls_realparents))
        ls_values.append(len(list_try))   
    return(ls_values)

#df_realp["proportions"] = df_realp["id"].apply(find_proportion_dated_nodes)
#df_realp[['dated_nodes', 'real_descendants', 'all_descendants']] = pd.DataFrame(df_realp['proportions'].to_list(), index=df_realp.index)

#df_realp.to_csv("df_nodes_with_proportion_of_dated&realparent.csv",encoding = "gbk")
