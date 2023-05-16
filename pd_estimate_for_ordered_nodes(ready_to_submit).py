#pd estimate for entire nodes table
import pandas as pd
import numpy as np
import os
import random
from random import choice
from random import choice
from sklearn.model_selection import train_test_split
os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")

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
        return(1/(df_nodes.iloc[a-1,5]-df_nodes.iloc[a-1,4]+1))

    
def ed_leaf(leaf_id):
    list_try = []#list of parents
    cp = leaves1.iat[int(leaf_id)-1,1]#
    ##find leaf parents
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1,1]
    list_try = sorted(list_try,reverse = True)
    #list_try is a list of parents sorted from young to old 
    ls_des = []
    ls_part = []
    ls_ed = []
    ind = 1
    list_try.insert(0,-1)
    while ind < len(list_try):
        if find_age_for_pd_estimate(list_try[ind]) == 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ind += 1
        if find_age_for_pd_estimate(list_try[ind]) > 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ed_temp = ((find_age_for_pd_estimate(list_try[ind])-find_age_for_pd_estimate(list_try[ind-len(ls_part)]))/len(ls_part))*sum(ls_des)
            ls_ed.append(ed_temp)
            ind += 1
            ls_part = []
            ls_des = []
    return(float(format(sum(ls_ed),'.6f')))



def ed_node(node_id):
    list_try = []#list of parents
    cp = df_nodes.iat[int(node_id)-1,1]
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1,1]
        #from young to old
    list_try.append(node_id)
    list_try = sorted(list_try,reverse = True)
    ls_des = []
    ls_part = []
    ls_ed = []
    ind = 1
    while ind < len(list_try):
        if find_age_for_pd_estimate(list_try[ind]) == 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ind += 1
        if find_age_for_pd_estimate(list_try[ind]) > 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ed_temp = ((find_age_for_pd_estimate(list_try[ind])-find_age_for_pd_estimate(list_try[ind-len(ls_part)]))/len(ls_part))*sum(ls_des)
            ls_ed.append(ed_temp)
            ind += 1
            ls_part = []
            ls_des = []
    return(float(format(sum(ls_ed),'.6f')))



##check if parents younger than descendants!!!

def find_closest_parents_with_date_PD(a):##find closest node parents that have a date
    #based on a node id(a)
    cp = nodes_for_age_function.iat[int(a)-1,1]#current parents
    while cp != -27400288:#not the first node
        date_estimate = find_age_for_pd_estimate(cp)
        if date_estimate > 0:
            return(date_estimate)
        else:
            cp = nodes_for_age_function.iat[int(cp)-1,1]
            #will return 4025 if no parents have age


df_leaves = pd.read_csv("ordered_leaves.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])


ages = pd.read_csv("latest_node_dates.csv", low_memory=False)
df_nodes = pd.read_csv("ordered_nodes.csv",low_memory=False)

#merge df_nodes with node ages
nodes_with_age = pd.merge(df_nodes, ages, how = "left",on = "id")

#1. select those nodes that have age(s)
nodes_with_age= nodes_with_age.fillna(0)


##arrange the date table:
#divide into 2,
#1 have only age, 1 have only ages
nodes_part1 = nodes_with_age.query("age != 0") #have age either from the ordered nodes table(age) or the json field(ages)
nodes_part2 = nodes_with_age.query("age == 0") #have age either from the ordered nodes table(age) or the json field(ages)

#deal with part 2, those have multiple ages
nodes_part3 = nodes_part2.query("ages != 0")
nodes_part3 = nodes_part3[['id','ages']]

ls_age = []
for row in nodes_part3.itertuples():
    age = getattr(row,"ages")
    if  ","  in age:#a list of dates
        ls_ages = age.split(",") #form a list of several ages
        age_selected = choice(ls_ages)#randomly select one from several ages
        ls_age.append(age_selected)
    else:#this selected node have only 1 date
        ls_age.append(age)

nodes_part3["age"] = ls_age
nodes_part3 = nodes_part3[['id','age']]
nodes_part1 = nodes_part1[['id','age']]
df_ages = pd.concat([nodes_part1,nodes_part3],axis=0)

nodes_no_age = df_nodes[['id','parent']]
nodes_for_age_function = pd.merge(nodes_no_age,df_ages, how = "left", on = "id")
nodes_for_age_function = nodes_for_age_function.fillna(0)
nodes_for_age_function.iat[0,2] = 4025.0
nodes_for_age_function["age"] = pd.to_numeric(nodes_for_age_function["age"])

#check nodes_for_age_function
for row in nodes_for_age_function.query("0 < age < 4025").itertuples():
    node_id = getattr(row,"id")
    age_p = find_closest_parents_with_date_PD(node_id)##return the date estimate of its closest parent
    node_age = getattr(row,"age")#age of itself
    if node_age > age_p:
        ls_select = []
        ls_select.append(age_p)
        ls_select.append(node_age)
        selected = choice(ls_select)#select one age to discard
        nodes_for_age_function["age"].replace(selected, 0)#the age of discarded node is 0     


##find_true PD for all the 23 selected groups, see if there is triphasic curve
#read in the 23 pd table

##do node ed for the 23 groups

##calculate misadded pd

#"total_pd_all.csv",
#merge based on name instead of id
df_pd23 = df_pd23.drop(index = 0)

#get nodeid for the 23 groups
ls_nodeid = [60673,63780,63336,122046,172687,543115,805308,972346,1082356,1452621,1595859,1882225,2056499,840050,840051,840165,841421,875861,889827,889596,899851,884549,889849]
df_pd23["node_id"] = ls_nodeid

df_ed = pd.read_csv("ed_boostrap(100).csv",low_memory=False)
ed_values = df_ed.drop(columns = ["Unnamed: 0","id","parent","sum_ed","ed"])

sum_ed = list(ed_actin.apply(lambda x: x.sum(), axis = 0))

#nodes:keep node_id, name, leaf_rgt,leaf_lft
nodes_with_pd = df_nodes[['id', 'name', 'ott', 'parent', 'leaf_lft', 'leaf_rgt']]
nodes_with_pd["richness"] = nodes_with_pd["leaf_rgt"]-nodes_with_pd["leaf_lft"]
nodes_with_pd["richness"] = nodes_with_pd.apply(lambda x:x["richness"]+1, axis = 1)
nodes_with_pd["ED_from_previous_nodes"] = nodes_with_pd["id"].apply(ed_node)

nodes_with_pd["misadded_pd"] = nodes_with_pd["ED_from_previous_nodes"]*nodes_with_pd["richness"]


#add "sum_ed", pd
#keep using the ed from bootstrap analysis
df_ed = pd.read_csv("ed_boostrap(100).csv",low_memory=False)
ed_values = df_ed.drop(columns = ["Unnamed: 0","id","parent","sum_ed","ed"])

ls_of_pd_list = []
for row in nodes_with_pd.itertuples():
    misadded_pd = getattr(row,"misadded_pd")
    lft = getattr(row,"leaf_lft")
    rgt = getattr(row,"leaf_rgt")
    ed_ranged = ed_values[lft-1:rgt]#index = leaf_id - 1
    sum_ed = list(ed_ranged.apply(lambda x: x.sum(), axis = 0))
    ls_pd = []
    for i in sum_ed:
        corrected_pd = i - misadded_pd
        ls_pd.append(corrected_pd)#should be a list of 100 pd scores
    ls_of_pd_list.append(ls_pd)

nodes_with_pd["pd"] = ls_of_pd_list

#each node will have 100 pd estimates

nodes_with_pd["pd"] = ls_of_pd_list

nodes_with_pd1 = nodes_with_pd["pd"].apply(pd.Series, index = list(range(0,100)))
nodes_with_pd1["median_PD"]=  nodes_with_pd1.median(axis=1)
df1 = pd.DataFrame(nodes_with_pd, columns = ["id","name","richness"])
nodes_with_pd_final = pd.concat([df1,nodes_with_pd1],axis = 1)










