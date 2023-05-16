import pandas as pd
import numpy as np
import os
import random
from random import choice
import time

os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")##change this to the path that the files existed in your computer

#read in leaves table and keep necessary columns. 
df_leaves = pd.read_csv("ordered_leaves.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])


#leaves table for calculating ed(leaves2 is a table of leaves in which species that have the same most
##recent ancestor are combined)

leaves2 = leaves1
leaves2['id'] = leaves2['id'].astype(str)
leaves2 = leaves2.groupby("parent")["id"].apply(lambda x:x.str.cat(sep = ",")).reset_index()


#read in nodes table.

    #nodes for finding parents
df_nodes = pd.read_csv("ordered_nodes.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

    #nodes for calculating ED
nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)


def find_node_parents(a):##find parents based on a node id(a)
    list_parentsn = []
    cp = nodes1.iat[int(a)-1,2]#current parents
    while cp != -27400288:
        list_parentsn.append(cp)
        cp = nodes1.iat[int(cp)-1,2]     
    return sorted(list_parentsn)


def find_closest_parents_with_date(a):##find closest node parents that have a date
    #based on a node id(a)
    cp = nodes1.iat[int(a)-1,2]#current parents
    while cp != -27400288:#not the first node
        #once cp in ages_selected，which means this id has a date
        if cp in ages_selected_with_parent["id"].values:
            date_estimate = find_age(cp)
            return(date_estimate)
        else:
            cp = nodes1.iat[int(cp)-1,2]
            #will return 4025 if no parents have age



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
    return(float(format(sum(ls_ed),'.6f')))





#an incomplete node table that will be merged with age table later
nodes_no_age = pd.DataFrame(nodes,columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0"])

##has been updated with the latest json data
ages = pd.read_csv("latest_node_dates.csv", low_memory=False)


list_of_ed_list = []
##main program(return a 1656825 ed scores list)

times = 0
while times < 100:
    start_time = time.time()
    random.seed(times)
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
    ages_selected = pd.DataFrame(ages_selected,columns = ['id', 'age'])
    ages_bootstrap_final = pd.merge(nodes_no_age, ages_selected, how = "left",on = "id")
    ages_bootstrap_final = ages_bootstrap_final.fillna(0)
    ages_bootstrap_final.iat[0,6] = 4025.0#give age to the root/node1, always here
    ages_bootstrap_final["age"] = pd.to_numeric(ages_bootstrap_final["age"])


##find parent for nodes in ages_selected
    ages_selected_with_parent= pd.merge(nodes_no_age,
                                    ages_selected, how = "left",on = "id")
    ages_selected_with_parent.iat[0,6] = 4025.0
    ages_selected_with_parent["age"] = pd.to_numeric(ages_selected_with_parent["age"])
    ages_selected_with_parent = ages_selected_with_parent[ages_selected_with_parent["age"]>0]


##see whether there is descendants that has a larger age than parents
    #randomnize the order
    ages_selected_reindexed = ages_selected.reindex(np.random.permutation(ages_selected.index))
    for row in ages_selected_reindexed.itertuples():
        node_id = getattr(row,"id")
        age_p = find_closest_parents_with_date(node_id)##return the date estimate of its closest parent
        node_age = eval(getattr(row,"age"))#age of itself
        if node_age > age_p:
            ls_select = []
            ls_select.append(age_p)
            ls_select.append(node_age)
            selected = choice(ls_select)#select one age to discard
            ages_bootstrap_final["age"].replace(selected, 0)#the age of discarded node is 0     

##combine the two species that have the same most recent common ancestor,可优化
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

    ##ccalculate ed for the entire table###############################
    leaves2["new_id"] = new_id_ls
    ed_total = leaves2["new_id"].apply(ed_new)
    #get the end time
    times = times+1
    end_time = time.time()
    print(times)
    print((end_time-start_time)/3600)
    list_of_ed_list.append(ed_total)

df_ed = pd.DataFrame(list_of_ed_list).transpose()
##a dataframe of ed scores

#df_ed.to_csv("df_ed.csv",encoding = "gbk")

df_ed.to_csv("ed_boostrap(100).csv",encoding = "gbk")


##





