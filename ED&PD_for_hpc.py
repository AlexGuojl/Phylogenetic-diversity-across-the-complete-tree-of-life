# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import os
import random
from random import choice
import time
from datetime import datetime

import sys




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


def ed_new3(leaf_id):
    list_try = []#list of parents
    ls_bls = []
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
            branch_len = (find_age(list_try[ind])-find_age(list_try[ind-len(ls_part)]))/len(ls_part)#这部分不一定对，可能是len+1
            ls_bls.append(branch_len)
            ls_ed.append(ed_temp)
            ind += 1
            ls_part = []
            ls_des = []
    return float(format(sum(ls_ed))),ls_bls[0]




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
   
ages_selected = ages.sample(frac=0.5, replace=False, random_state = arg)
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
ed_total = leaves2["new_id"].apply(ed_new3)
ls_ed_total = ed_total.tolist()
ls_ed,ls_ed_term =  zip(*ls_ed_total)

print(ls_ed)
print(ls_ed_term)

#next:estimate PD for all "real" nodes

#make ed list into a dataframe with 2235076 rows
df_ed = pd.DataFrame(ls_ed)
df_ed.columns = ["ed"]
ed_with_id = pd.concat([df_ed, leaves2], axis=1)
ed_values_all = pd.merge(leaves1,ed_with_id,how = "left",on = "parent")
ed_values = pd.DataFrame(ed_values_all,columns = ["ed"])
#now a dataframe with ed values and corresponding info is generated


#通过ages_bootstrap_final获得nodes_for_age_function，进而计算pd值
nodes_for_age_function = pd.DataFrame(ages_bootstrap_final,columns = ["id","parent","age"])

def find_age_for_pd_estimate(a):#use the nodes_with_age table, "a" refers to a node id
    if a == -1:
        return(0)
    if float(nodes_for_age_function.iloc[a-1,2]) > 0:
        return(float(nodes_for_age_function.iloc[a-1,2]))#age get from "age" column
    else:
        return(0)


def find_des(a): ##return 1/descendants for a node id in the whole table
    if a == -1:
        return(1)
    else:
        return(1/(df_nodes.iloc[a-1,6]-df_nodes.iloc[a-1,5]+1))

def find_missing_PD(node_id):#
    if node_id == 1:
        return(0)
    node_id = nodes_for_age_function.iat[int(node_id)-1,1]#exclude the unique short branch

    if find_age_for_pd_estimate(node_id) > 0:
        return(float(4025-find_age_for_pd_estimate(node_id)))
    else:
        df_des_node = df_nodes[df_nodes["parent"] == node_id]#This is the first generation of nodes with node_id as parent
                                                             #The purpose of this step is to find a close node date in order to estimate the branch length.
        ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)#a list of age estimate
        ls_des_age = sorted(ls_des_age,reverse = True)
        count_up = 0
        count_down = 0
        if ls_des_age == []:#most recent common ancestor already,start counting up
            ls_des_age.append(0)
            count_down += 1
        if max(ls_des_age) > 0:
            count_down += 1
        else:
            #print("keep finding")
            while True:
                df_des_node= df_nodes[df_nodes["parent"].isin(list(df_des_node["id"]))]
                ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
                ls_des_age = sorted(ls_des_age,reverse = True)
                count_down += 1

                if ls_des_age == []:
                    ls_des_age.append(0)
                    count_down += 1
                    break
                if max(ls_des_age)== 0:
                   # print("0")
                    count_down += 1
                    continue
                if max(ls_des_age) > 0:
                    count_down += 1
                    
                    #ls_des_age.append(max(ls_des_age))
                    break
    #print(count_down)
    #return(ls_des_age)
    parent_for_node_id = nodes_for_age_function.iat[int(node_id)-1,1]
    while True:
        if find_age_for_pd_estimate(parent_for_node_id) > 0:
            count_up += 1
        #now, calculate!
            BL = (find_age_for_pd_estimate(parent_for_node_id)-max(ls_des_age))/(count_up+count_down)
    #print(BL)
            node_date = find_age_for_pd_estimate(parent_for_node_id)-BL*count_up
            break
        else:
            count_up += 1
            parent_for_node_id = nodes_for_age_function.iat[int(parent_for_node_id)-1,1]
            continue
    #print(count_up)
    #print(count_down)
    return(4025-node_date)
#this function gives the ed estimate of the *******parent of this node_id******
    #the terminal branch will be weighted too in order to estimate PD easier

def ed_node_realp(node_id): #all the id are nodes, so no need to use leaf_realparent、
    if node_id== 1:
        return(0)
    node_id = df_nodes.iat[int(node_id)-1,2]
    if find_age_for_pd_estimate(node_id)>0:
        list_try = []#list of parents
        cp = df_nodes.iat[int(node_id)-1,2]
        while cp != -27400288:##not the root(oldest node)
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
        list_try.append(node_id)
        list_try = sorted(list_try,reverse = True)
        ls_des = []
        ls_part = []
        ls_ed = []
        ind = 1
        while ind < len(list_try):
            if float(find_age_for_pd_estimate(list_try[ind])) == 0:
                ls_part.append(list_try[ind])
                ls_des.append(find_des(list_try[ind-1]))
                ind += 1
            if float(find_age_for_pd_estimate(list_try[ind])) > 0:
                ls_part.append(list_try[ind])
                ls_des.append(find_des(list_try[ind-1]))
                ed_temp = ((find_age_for_pd_estimate(list_try[ind])-find_age_for_pd_estimate(list_try[ind-len(ls_part)]))/len(ls_part))*sum(ls_des)
                ls_ed.append(ed_temp)
                ind += 1
                ls_part = []
                ls_des = []
    #print(ls_ed)
        return(float(format(sum(ls_ed),'.6f')))

    if find_age_for_pd_estimate(node_id) == 0:
        list_try = []#list of parents
        cp = df_nodes.iat[int(node_id)-1,2]
        while cp != -27400288:##not the root(oldest node)
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
        list_try.append(node_id)
        list_try = sorted(list_try,reverse = True)
        ls_des = []
        ls_part = []
        ls_ed = []
        ind = 1
        #####ccounter——times
        times = 0
        while ind < len(list_try):
            if float(find_age_for_pd_estimate(list_try[ind])) == 0:
                ls_part.append(list_try[ind])
                ls_des.append(find_des(list_try[ind-1]))
                ind += 1
            if float(find_age_for_pd_estimate(list_try[ind])) > 0:
                if times == 0:
                    ls_part.append(list_try[ind])
                    ls_des.append(find_des(list_try[ind-1]))
                    ed_temp = ((find_age_for_pd_estimate(list_try[ind])-(4025-find_missing_PD(node_id)))/len(ls_part))*sum(ls_des)
                    ls_ed.append(ed_temp)
                    ind += 1
                    times +=1
                    ls_part = []
                    ls_des = []
                else:
                    ls_part.append(list_try[ind])
                    ls_des.append(find_des(list_try[ind-1]))
                    ed_temp = ((find_age_for_pd_estimate(list_try[ind])-find_age_for_pd_estimate(list_try[ind-len(ls_part)]))/len(ls_part))*sum(ls_des)
                    ls_ed.append(ed_temp)
                    ind += 1
                    ls_part = []
                    ls_des = []
    #print(ls_ed)
    return(float(format(sum(ls_ed),'.6f')))



nodes_with_pd = df_nodes[["id","name","ott","parent","leaf_lft","leaf_rgt"]]
nodes_with_pd["richness"] = nodes_with_pd["leaf_rgt"]-nodes_with_pd["leaf_lft"]+1
ls_realp_leaf = list(df_leaves["real_parent"])
ls_realp_node = list(df_nodes["real_parent"])
ls_realp =  list(set(ls_realp_leaf+ls_realp_node))
nodes_with_pd = nodes_with_pd[nodes_with_pd['id'].isin(ls_realp)]
#calculate pd for all real nodes
nodes_with_pd["ED_from_previous_nodes"] = nodes_with_pd["id"].apply(ed_node_realp)
nodes_with_pd["misadded_pd"] = nodes_with_pd["ED_from_previous_nodes"]*nodes_with_pd["richness"]

ls_pd = []
for row in nodes_with_pd.itertuples():
    misadded_pd = getattr(row,"misadded_pd")
    lft = getattr(row,"leaf_lft")
    rgt = getattr(row,"leaf_rgt")
    ed_ranged = ed_values[lft-1:rgt]#index = leaf_id - 1
    ed_ranged = list(ed_ranged["ed"])
    sum_ed = sum(ed_ranged)###sum of ed scores
    corrected_pd = sum_ed - misadded_pd
    ls_pd.append(corrected_pd)#should be a list of 100 pd scores

print(ls_pd)




















