#pd estimate for entire nodes table
iimport pandas as pd
import numpy as np
import os
import random
import time
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
        return(1/(df_nodes.iloc[a-1,6]-df_nodes.iloc[a-1,5]+1))

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
                    #print(ls_des_age[0])
                    #print(count)
                    break

        list_try = []#list of parents
        cp = df_nodes.iat[int(node_id)-1,2]
        while cp != -27400288:##not the root(oldest node)
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1,2]
        #from young to old
        list_try.append(node_id)
        #print(list_try)
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
                if find_age_for_pd_estimate(list_try[ind-len(ls_part)]) == 0:
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
    #list_try = sorted(list(set(ls_realp_node) & set(list_try)),reverse = True)#list of real parent
    list_date = [find_age_for_pd_estimate(i) for i in list_try]
    return([i for i in list_date if i > 0])



df_leaves = pd.read_csv("updated_ordered_leaves.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])

df_ed = pd.read_csv("ed_boostrap_new2(100).csv",low_memory=False) 
ed_values = df_ed.drop(columns = ["id","parent","ed","sum_ed","ott","Unnamed: 0"])
ages = pd.read_csv("latest_node_dates(real_parent_only).csv", low_memory=False)
df_nodes = pd.read_csv("updated_ordered_nodes.csv",low_memory=False)

##list of real parent
ls_realp_leaf = list(df_leaves["real_parent"])
ls_realp_node = list(df_nodes["real_parent"])
ls_realp =      list(set(ls_realp_leaf+ls_realp_node))

#merge df_nodes with node ages
nodes_with_age = pd.merge(df_nodes, ages, how = "left",on = "id")
#1. select those nodes that have age(s)
nodes_with_age= nodes_with_age.fillna(0)


##arrange the date table:
#divide into 2,
#1 have only age, 1 have only ages
nodes_part1 = nodes_with_age.query("age != 0") #have age from the ordered nodes table(age)
nodes_part2 = nodes_with_age.query("age == 0") #have age either from the ordered nodes table(age) or the json field(ages)

#deal with part 2, those have multiple ages
nodes_part3 = nodes_part2.query("ages != 0")
nodes_part3 = nodes_part3[['id','ages']]

#randomly select an age for nodes that have multiple ages 
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
for row in nodes_for_age_function[nodes_for_age_function["age"]>0][1:].itertuples():    
    node_id = getattr(row,"id")
    lsdates =  find_parents_with_date_PD(node_id)
    lsdates1 = sorted(lsdates)
    start_time = time.time()   
    if lsdates == lsdates1:
        continue
    else:
        i = 0
        ls_chose = []
        while i < len(lsdates):
            for j in range(1+i,len(lsdates)):
                if lsdates[i]<=lsdates[j]:
                    continue
                if lsdates[i]>lsdates[j]:
                    ls_chose.append(lsdates[i])
                    ls_chose.append(lsdates[j])
                    selected = choice(ls_chose)
                    if selected == lsdates[j]:#find the index,replace it in nodes table
                        inds = nodes_for_age_function[nodes_for_age_function.age == selected].index.tolist()
                        for k in inds:
                            nodes_for_age_function.at[k,"age"] = 0
                        ls_chose = []
                    if selected == lsdates[i]:#find the index,replace it in nodes table
                        inds = nodes_for_age_function[nodes_for_age_function.age == selected].index.tolist()
                        for k in inds:
                            nodes_for_age_function.at[k,"age"] = 0
                        ls_chose = []
            i += 1
    end_time = time.time()


#check nodes_for_age_function
##now a nodes table for pd estimate is prepared
##check
nodes_with_pd = df_nodes[['id', 'name', 'ott', 'parent', 'leaf_lft', 'leaf_rgt']]
nodes_with_pd["richness"] = nodes_with_pd["leaf_rgt"]-nodes_with_pd["leaf_lft"]
nodes_with_pd["richness"] = nodes_with_pd.apply(lambda x:x["richness"]+1, axis = 1)

##ccalculate ed for the entire table###############################
nodes_with_pd["id"] = nodes_with_pd["id"].astype(int)

pd_24 =nodes_with_pd.query("id == 1|id ==60673|id ==63780|id ==63336|id ==122046|id ==122046|id ==172687|id ==543115|id ==805308|id ==972346|id ==1082356|id ==1452621|id ==1595859|id ==1882225|id ==2056499|id ==840050|id ==840051|id ==840165|id ==841421|id ==875861|id ==889827|id ==889596|id ==889849|id ==884549 |id == 899851")





