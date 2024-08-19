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


#read in leaves table and keep necessary columns.

arg = int(sys.argv[1])#receive parameter

df_leaves = pd.read_csv("updated_ordered_leaves_2.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])


#leaves table for calculating ed(leaves2 is a table of leaves in which species that have the same most
##recent ancestor are combined)

leaves2 = leaves1
leaves2['id'] = leaves2['id'].astype(str)
leaves2 = leaves2.groupby("parent")["id"].apply(lambda x:x.str.cat(sep = ",")).reset_index()

new_id_ls = []

for row in leaves2.itertuples():
    leaf_id = getattr(row,"id")#can be 1 or a list of node date estimates
    if  ","  in leaf_id:
        ls_leaf_id = leaf_id.split(",")
        leaf_id = int(ls_leaf_id[0])
        new_id_ls.append(leaf_id)
    else:
        leaf_id = int(leaf_id)
        new_id_ls.append(leaf_id)

leaves2["new_id"] = new_id_ls

#read in nodes table.

    #nodes for finding parents
df_nodes = pd.read_csv("updated_ordered_nodes_2.0.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

    #nodes for calculating ED
nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)


def find_parents_with_date(node_id):##return a list of date
    #based on a node id(a)
    list_try = []#list of parents
    cp = df_nodes.iat[int(node_id)-1,2]
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
    list_try.append(node_id)
    list_try = sorted(list_try,reverse = True)
    #list_try = sorted(list(set(ls_realp_node) & set(list_try)),reverse = True)#list of real parent
    list_date = [find_age(i) for i in list_try]
    return([i for i in list_date if i > 0])

def find_des(a): ##return 1/descendants for a node id in the whole table
    if a == -1:
        return(1)
    else:
        return(1/(nodes.iloc[a-1,4]-nodes.iloc[a-1,3]+1))

def find_age(a):#find node age
    if a == -1:
        return(0)
    else:
        return(ages_bootstrap_final.iloc[a-1,6])


def ed_new(leaf_id):
    list_try = []#list of parents
    cp = leaves1.iat[int(leaf_id)-1,1]
    ##find leaf parents
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = nodes.iat[int(cp)-1,2]
    list_try = sorted(list_try,reverse = True)
    #print(list_try)
    #list_try is a list of parents sorted from young to old 
    ls_des = []
    ls_part = []
    ls_ed = []
    ind = 1
    list_try.insert(0,-1)
    while ind < len(list_try):
        if find_age(list_try[ind]) == 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ind += 1
        if find_age(list_try[ind]) > 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ed_temp = ((find_age(list_try[ind])-find_age(list_try[ind-len(ls_part)]))/len(ls_part))*sum(ls_des)
            ls_ed.append(ed_temp)
            ind += 1
            ls_part = []
            ls_des = []
        #print(ls_des)
        #print(ls_part)
    return(float(format(sum(ls_ed),'.6f')))

##has been updated with the latest json data
nodes_no_age = pd.DataFrame(nodes,columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0"])

##always been updated with the latest json data
ages = pd.read_csv("latest_node_dates(real_parent)_2.0.csv", low_memory=False)

#list_of_ed_list = []
##main program(return a 1656825 ed scores list)



# Get the command line arguments


#print(arg)
# Print the arguments
#print("Arguments passed to the script:")
#for arg in script_args:
#print(arg)
random.seed(arg)
#    currentDateAndTime = datetime.now()
   # print(currentDateAndTime.strftime("%H:%M:%S"))
   
ages_selected = ages.sample(frac=0.5, replace=False, random_state = None)
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
##see whether there is descendants that has a larger age than parents

for row in ages_bootstrap_final[ages_bootstrap_final["age"]>0][1:].itertuples():
    node_id = int(getattr(row,"id"))
    lsdates =  find_parents_with_date(node_id)
    lsdates1 = sorted(lsdates)
    if lsdates == lsdates1:
        continue
    else:
        i = 0
        ls_chose = []
        while i < len(lsdates):
            for j in range(1+i,len(lsdates)):
                if lsdates[i] <= lsdates[j]:
                    continue
                if lsdates[i]>lsdates[j]:
                    ls_chose.append(lsdates[i])
                    ls_chose.append(lsdates[j])
                    selected = choice(ls_chose)
                    if selected == lsdates[j]:#find the index,replace it in nodes table
                        inds =  ages_bootstrap_final[ ages_bootstrap_final.age == selected].index.tolist()
                        for k in inds:
                            ages_bootstrap_final.at[k,"age"] = 0
                        ls_chose = []
                    if selected == lsdates[i]:#find the index,replace it in nodes table
                        inds =  ages_bootstrap_final[ ages_bootstrap_final.age == selected].index.tolist()
                        for k in inds:
                             ages_bootstrap_final.at[k,"age"] = 0
                        ls_chose = []
            i+=1

##combine the two species that have the same most recent common ancestor
#leaves table for calculating ed(leaves2 is a table of leaves in which species that have the same real_parent are combined)

    ##ccalculate ed for the entire table
ed_total = leaves2["new_id"].apply(ed_new)
ls_ed_total = ed_total.tolist()

print(ls_ed_total)



