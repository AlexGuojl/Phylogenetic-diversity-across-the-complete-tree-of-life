#threatened pd for certain clades(jawed vertebrates and tetrapods)

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

df_leaves = pd.read_csv("updated_ordered_leaves.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])
df_nodes = pd.read_csv("updated_ordered_nodes.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)

df_ed = pd.read_csv("ed_boostrap_new2(100).csv",low_memory=False) 
ed_values = df_ed.drop(columns = ["id","parent","ed","sum_ed","ott","Unnamed: 0"])


def find_des(a): ##return 1/descendants for a node id in the whole table
    if a == -1:
        return(1)
    else:
        return(1/(df_nodes.iloc[a-1,6]-df_nodes.iloc[a-1,5]+1))


def find_parents_with_date_PD(node_id):##find closest node parents that have a date
    #based on a node id(a)
    list_try = []#list of parents
    cp = df_nodes.iat[int(node_id)-1,2]
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
    list_try.append(node_id)
    list_try = sorted(list_try,reverse = True)
    
    list_date = [find_age_for_pd_estimate(i) for i in list_try]
    return([i for i in list_date if i > 0])

def find_age_for_pd_estimate(a):#use the nodes_with_age table, "a" refers to a node id
    if a == -1:
        return(0)
    if ages_bootstrap_final.iloc[a-1,6] > 0:
        return(ages_bootstrap_final.iloc[a-1,6])#age get from "age" column
    else:
        return(0)


def ed_terminal(leaf_id):###try must be sorted from big to small
    list_try = []#list of parents
    cp = leaves1.iat[int(leaf_id)-1,1]
    ##find leaf parents
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = nodes1.iat[int(cp)-1,2]
    list_try = sorted(list_try,reverse = True)
    #print(list_try)
    count = 1
    for i in list_try:
        if find_age_for_pd_estimate(i) == 0:
            count += 1
        else:
            ed_terminal = find_age_for_pd_estimate(i)/count
            break
    return (ed_terminal)


def node_terminal(node_id):###checked correct
    if find_age_for_pd_estimate(node_id)>0:
        return (find_age_for_pd_estimate(node_id))
    if find_age_for_pd_estimate(node_id) == 0:    
        list_try = []#list of parents
        cp = nodes1.iat[int(node_id)-1,2]
    ##find leaf parents
        while cp != -27400288:##not the root(oldest node)
            list_try.append(cp)
            cp = nodes1.iat[int(cp)-1,2]
        list_try = sorted(list_try,reverse = True)  
       # print(list_try)
        count = 2
        for i in list_try:
            if find_age_for_pd_estimate(i) == 0:
                count += 1
            else:
                ed_terminal = find_age_for_pd_estimate(i)/count
                break
   # real_parent = list_try[0]
    #if real_parent in list(nodes1["parent"]):
        return (ed_terminal)


def ed_node_realp(node_id): #all the id are nodes, so no need to use leaf_realparent
    if find_age_for_pd_estimate(node_id)== 4025:
        return(0)
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
        df_des_node = df_nodes[df_nodes["parent"] == node_id]
        ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
        ls_des_age = sorted(ls_des_age,reverse = True)
        count = 0
        if ls_des_age == []:
            ls_des_age.append(0)
            count+=1
        if ls_des_age[0] > 0:
            count += 1
        else:
            while True:
                df_des_node= df_nodes[df_nodes["parent"].isin(list(df_des_node["id"]))]
                ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
                ls_des_age = sorted(ls_des_age,reverse = True)
                count += 1
                if ls_des_age == []:
                    ls_des_age.append(0)
                    break
                if ls_des_age[0] > 0:
                    break

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
                if find_age_for_pd_estimate(list_try[ind-len(ls_part)]) == 0:#第一次出现age>0的情况
                    ed_temp = ((find_age_for_pd_estimate(list_try[ind])-ls_des_age[0])/(len(ls_part)+count))*sum(ls_des)
                    ls_ed.append(ed_temp)
                    ind += 1
                    ls_part = []
                    ls_des = []   
                else:
                    ed_temp = ((find_age_for_pd_estimate(list_try[ind])-find_age_for_pd_estimate(list_try[ind-len(ls_part)]))/len(ls_part))*sum(ls_des)
                    ls_ed.append(ed_temp)
                    ind += 1
                    ls_part = []
                    ls_des = []
        return(float(format(sum(ls_ed),'.6f')))


def find_all_descendants_node(node_id):
    leaf_lft =df_nodes.iloc[node_id-1,5]
    leaf_rgt =df_nodes.iloc[node_id-1,6]
    return(list(range(leaf_lft,leaf_rgt+1)))

def find_closest_parent_node(node_id):
    return(df_nodes.iloc[node_id-1,2])


def sum_ed_clade(node_id):
    lft = nodes1.iloc[node_id-1,5]
    rgt = nodes1.iloc[node_id-1,6]
    ed_ranged = ed_values[lft-1:rgt]
    sum_ed = list(ed_ranged.apply(lambda x: x.sum(), axis = 0))
    return(sum_ed)

def interial_branch_length(node_id): #all the id are nodes, so no need to use leaf_realparent
    if find_age_for_pd_estimate(node_id)>0:
        list_try = []#list of parents
        cp = df_nodes.iat[int(node_id)-1,2]
        while cp != -27400288:##not the root(oldest node)
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
        list_try.append(node_id)
        list_try = sorted(list_try,reverse = True)
        count = 1
        for i in list_try:
            if find_age_for_pd_estimate(i) == 0:
                count += 1
            else:
                bl_int = find_age_for_pd_estimate(i)/count
                return(bl_int)
            
    if find_age_for_pd_estimate(node_id) == 0:
        df_des_node = df_nodes[df_nodes["parent"] == node_id]
        ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
        ls_des_age = sorted(ls_des_age,reverse = True)
        count = 0
        if ls_des_age == []:
            ls_des_age.append(0)
            count += 1        
        if ls_des_age[0] > 0:
            count += 1
        else:
            while True:
                df_des_node= df_nodes[df_nodes["parent"].isin(list(df_des_node["id"]))]
                ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
                ls_des_age = sorted(ls_des_age,reverse = True)
                count += 1
                if ls_des_age == []:
                    ls_des_age.append(0)
                    break
                if ls_des_age[0] > 0:
                    break
        list_try = []#list of parents
        cp = df_nodes.iat[int(node_id)-1,2]
        while cp != -27400288:##not the root(oldest node)
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
        list_try.append(node_id)
        list_try = sorted(list_try,reverse = True)
        num = count+ 1
        for i in list_try:
            if find_age_for_pd_estimate(i) == 0:
                num += 1
            else:
                bl_int = (find_age_for_pd_estimate(i)-ls_des_age[0])/num
                return(bl_int)



def threatened_pd_tes1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_tes1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_tes1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_tes2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_tes2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_tes2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))
    
#squ,act,cho,cro,amp
def threatened_pd_squ1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_squ1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_squ1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_squ2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_squ2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_squ2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_act1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_act1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_act1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_act2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_act2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_act2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))


def threatened_pd_cho1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_cho1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_cho1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_cho2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_cho2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_cho2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_cro1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_cro1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_cro1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_cro2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_cro2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_cro2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_amp1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_amp1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_amp1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))
def threatened_pd_amp2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_amp2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_amp2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))



def threatened_pd_mam1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_mam1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_mam1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

    
def threatened_pd_mam2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_mam2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_mam2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_ave1(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_ave1["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_ave1["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

def threatened_pd_ave2(node_id):
    ls_pdnode = []
    ls_alldes = find_all_descendants_node(node_id)
    if set(ls_alldes).issubset(set(list(df_ave2["id"]))):
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
        if set(ls_alldes2).issubset(set(list(df_ave2["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    else:
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))

getparent = pd.DataFrame(leaves1,columns = ["id","parent","real_parent"])

df_tes1 = pd.read_csv("threatened_test.csv",low_memory=False)
df_tes1=df_tes1[df_tes1["status_code"] != "NT"]

df_tes2 = pd.read_csv("threatened_test_with_DD.csv",low_memory=False)
df_tes2=df_tes2[df_tes2["status_code"] != "NT"]

df_squ1 = pd.read_csv("threatened_squ.csv",low_memory=False)
df_squ1=df_squ1[df_squ1["status_code"] != "NT"]

df_squ2 = pd.read_csv("threatened_squ_with_DD.csv",low_memory=False)
df_squ2=df_squ2[df_squ2["status_code"] != "NT"]

df_act1 = pd.read_csv("threatened_act.csv",low_memory=False)
df_act1=df_act1[df_act1["status_code"] != "NT"]
df_act2 = pd.read_csv("threatened_act_with_DD.csv",low_memory=False)
df_act2=df_act2[df_act2["status_code"] != "NT"]

df_cho1 = pd.read_csv("threatened_chon.csv",low_memory=False)
df_cho1=df_cho1[df_cho1["status_code"] != "NT"]
df_cho2 = pd.read_csv("threatened_chon_with_DD.csv",low_memory=False)
df_cho2=df_cho2[df_cho2["status_code"] != "NT"]

df_cro1 = pd.read_csv("threatened_croc.csv",low_memory=False)
df_cro1=df_cro1[df_cro1["status_code"] != "NT"]

df_cro2 = pd.read_csv("threatened_croc_with_DD.csv",low_memory=False)
df_cro2=df_cro2[df_cro2["status_code"] != "NT"]

df_amp1 = pd.read_csv("threatened_amp.csv",low_memory=False)
df_amp1=df_amp1[df_amp1["status_code"] != "NT"]

df_amp2 = pd.read_csv("threatened_amp_with_DD.csv",low_memory=False)
df_amp2=df_amp2[df_amp2["status_code"] != "NT"]

df_mam1 = pd.read_csv("threatened_mammals.csv",low_memory=False)
df_mam1=df_mam1[df_mam1["status_code"] != "NT"]

df_mam2 = pd.read_csv("threatened_mammals_w.csv",low_memory=False)
df_mam2=df_mam2[df_mam2["status_code"] != "NT"]

df_ave1 = pd.read_csv("threatened_ave.csv",low_memory=False)
df_ave1=df_ave1[df_ave1["status_code"] != "NT"]

df_ave2 = pd.read_csv("threatened_aves_w.csv",low_memory=False)
df_ave2=df_ave2[df_ave2["status_code"] != "NT"]



df_mam1 = pd.merge(df_mam1,getparent,how = "left",on = "id")
df_mam2 = pd.merge(df_mam2,getparent,how = "left",on = "id")
df_ave1 = pd.merge(df_ave1,getparent,how = "left",on = "id")
df_ave2 = pd.merge(df_ave2,getparent,how = "left",on = "id")
df_tes1 = pd.merge(df_tes1,getparent,how = "left",on = "id")
df_tes2 = pd.merge(df_tes2,getparent,how = "left",on = "id")
df_squ1 = pd.merge(df_squ1,getparent,how = "left",on = "id")
df_squ2 = pd.merge(df_squ2,getparent,how = "left",on = "id")
df_act1 = pd.merge(df_act1,getparent,how = "left",on = "id")
df_act2= pd.merge(df_act2,getparent,how = "left",on = "id")
df_cho1= pd.merge(df_cho1,getparent,how = "left",on = "id")
df_cho2= pd.merge(df_cho2,getparent,how = "left",on = "id")
df_cro1= pd.merge(df_cro1,getparent,how = "left",on = "id")
df_cro2= pd.merge(df_cro2,getparent,how = "left",on = "id")
df_amp1= pd.merge(df_amp1,getparent,how = "left",on = "id")
df_amp2= pd.merge(df_amp2,getparent,how = "left",on = "id")


df_parent_tes1 = pd.DataFrame({"parent": list(set(list(df_tes1["parent"])))})
df_parent_tes2= pd.DataFrame({"parent": list(set(list(df_tes2["parent"])))})
df_parent_squ1 = pd.DataFrame({"parent": list(set(list(df_squ1["parent"])))})
df_parent_squ2 = pd.DataFrame({"parent": list(set(list(df_squ2["parent"])))})
df_parent_act1 = pd.DataFrame({"parent": list(set(list(df_act1["parent"])))})
df_parent_act2 = pd.DataFrame({"parent": list(set(list(df_act2["parent"])))})
df_parent_cho1 = pd.DataFrame({"parent": list(set(list(df_cho1["parent"])))})
df_parent_cho2 = pd.DataFrame({"parent": list(set(list(df_cho2["parent"])))})
df_parent_cro1 = pd.DataFrame({"parent": list(set(list(df_cro1["parent"])))})
df_parent_cro2 = pd.DataFrame({"parent": list(set(list(df_cro2["parent"])))})
df_parent_amp1 = pd.DataFrame({"parent": list(set(list(df_amp1["parent"])))})
df_parent_amp2 = pd.DataFrame({"parent": list(set(list(df_amp2["parent"])))})
df_parent_mam1 = pd.DataFrame({"parent": list(set(list(df_mam1["parent"])))})
df_parent_mam2 = pd.DataFrame({"parent": list(set(list(df_mam2["parent"])))})
df_parent_ave1 = pd.DataFrame({"parent": list(set(list(df_ave1["parent"])))})
df_parent_ave2 = pd.DataFrame({"parent": list(set(list(df_ave2["parent"])))})



nodes_no_age = pd.DataFrame(nodes,columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0"])

##always been updated with the latest json data
ages = pd.read_csv("latest_node_dates(real_parent_only).csv", low_memory=False)

ls_tes1 = []
ls_squ1 = []
ls_act1 = []
ls_cho1 = []
ls_cro1 = []
ls_amp1 = []

ls_tes2 = []
ls_squ2 = []
ls_act2 = []
ls_cho2 = []
ls_cro2 = []
ls_amp2 = []
##main program(return a ed scores list)
ls_mam1 = []
ls_mam2 = []
ls_ave1 = []
ls_ave2 = []


start_time = time.time()
currentDateAndTime = datetime.now()
# print(currentDateAndTime.strftime("%H:%M:%S")) 
ages_selected = ages #.sample(frac=0.5, replace=False, random_state = None)
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
    lsdates =  find_parents_with_date_PD(node_id)
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



ls_threatened_pd_tes1  = df_parent_tes1["parent"].apply(threatened_pd_tes1)
ls_threatened_pd_tes2  = df_parent_tes2["parent"].apply(threatened_pd_tes2)
ls_threatened_pd_squ1  = df_parent_squ1["parent"].apply(threatened_pd_squ1)
ls_threatened_pd_squ2  = df_parent_squ2["parent"].apply(threatened_pd_squ2)
ls_threatened_pd_act1  = df_parent_act1["parent"].apply(threatened_pd_act1)
ls_threatened_pd_act2  = df_parent_act2["parent"].apply(threatened_pd_act2)
ls_threatened_pd_cho1  = df_parent_cho1["parent"].apply(threatened_pd_cho1)
ls_threatened_pd_cho2  = df_parent_cho2["parent"].apply(threatened_pd_cho2)
ls_threatened_pd_cro1  = df_parent_cro1["parent"].apply(threatened_pd_cro1)
ls_threatened_pd_cro2  = df_parent_cro2["parent"].apply(threatened_pd_cro2)
ls_threatened_pd_amp1  = df_parent_amp1["parent"].apply(threatened_pd_amp1)
ls_threatened_pd_amp2  = df_parent_amp2["parent"].apply(threatened_pd_amp2)

ls_threatened_pd_mam1  = df_parent_mam1["parent"].apply(threatened_pd_mam1)
ls_threatened_pd_mam2  = df_parent_mam2["parent"].apply(threatened_pd_mam2)
ls_threatened_pd_ave1  = df_parent_mam1["parent"].apply(threatened_pd_ave1)
ls_threatened_pd_ave2  = df_parent_mam1["parent"].apply(threatened_pd_ave2)

ls_mam1.append(sum(ls_threatened_pd_mam1))
ls_mam2.append(sum(ls_threatened_pd_mam2))
ls_ave1.append(sum(ls_threatened_pd_ave1))
ls_ave2.append(sum(ls_threatened_pd_ave2))


ls_tes1.append(sum(ls_threatened_pd_tes1))
ls_squ1.append(sum(ls_threatened_pd_squ1))
ls_act1.append(sum(ls_threatened_pd_act1)) 
ls_cho1.append(sum(ls_threatened_pd_cho1))
ls_cro1.append(sum(ls_threatened_pd_cro1))
ls_amp1.append(sum(ls_threatened_pd_amp1))

ls_tes2.append(sum(ls_threatened_pd_tes2))
ls_squ2.append(sum(ls_threatened_pd_squ2))
ls_act2.append(sum(ls_threatened_pd_act2))
ls_cho2.append(sum(ls_threatened_pd_cho2))
ls_cro2.append(sum(ls_threatened_pd_cro2))
ls_amp2.append(sum(ls_threatened_pd_amp2))




df_tes1 = pd.DataFrame(ls_tes1)
df_tes1.to_csv("threatened_pdtestudines.csv",encoding = "gbk")
df_tes2 = pd.DataFrame(ls_tes2)
df_tes2.to_csv("threatened_pdtestudines_w.csv",encoding = "gbk")

df_squ1 = pd.DataFrame(ls_squ1)
df_squ1.to_csv("threatened_pdsquamata.csv",encoding = "gbk")
df_squ2 = pd.DataFrame(ls_squ2)
df_squ2.to_csv("threatened_pdsquamata_w.csv",encoding = "gbk")

df_act1 = pd.DataFrame(ls_act1)
df_act1.to_csv("threatenedpd_ray_fin.csv",encoding = "gbk")
df_act2 = pd.DataFrame(ls_act2)
df_act2.to_csv("threatenedpd_ray_fin_w.csv",encoding = "gbk")

df_cho1 = pd.DataFrame(ls_cho1)
df_cho1.to_csv("threatenedpd_chond.csv",encoding = "gbk")
df_cho2 = pd.DataFrame(ls_cho2)
df_cho2.to_csv("threatenedpd_chond_w.csv",encoding = "gbk")

df_cro1 = pd.DataFrame(ls_cro1)
df_cro1.to_csv("threatenedpd_cro.csv",encoding = "gbk")
df_cro2 = pd.DataFrame(ls_cro2)
df_cro2.to_csv("threatenedpd_cro_w.csv",encoding = "gbk")

df_amp1 = pd.DataFrame(ls_amp1)
df_amp1.to_csv("threatenedpd_amp.csv",encoding = "gbk")
df_amp2 = pd.DataFrame(ls_amp2)
df_amp2.to_csv("threatenedpd_amp_w.csv",encoding = "gbk")

df_mam1 = pd.DataFrame(ls_mam1)
df_mam1.to_csv("threatenedpd_mammals.csv",encoding = "gbk")
df_mam2 = pd.DataFrame(ls_mam2)
df_mam2.to_csv("threatenedpd_mammals_w.csv",encoding = "gbk")

df_ave1 = pd.DataFrame(ls_ave1)
df_ave1.to_csv("threatenedpd_aves.csv",encoding = "gbk")
df_ave2 = pd.DataFrame(ls_ave2)
df_ave2.to_csv("threatenedpd_aves_w.csv",encoding = "gbk")

end_time = time.time()
print((end_time-start_time)/3600)
print(currentDateAndTime.strftime("%H:%M:%S"))

