#pd estimate for entire nodes table
import pandas as pd
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
        #####
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

def find_missing_PD(node_id):#checked accurate already!!
    if find_age_for_pd_estimate(node_id) > 0:
        return(float(4025-find_age_for_pd_estimate(node_id)))
    else:
        df_des_node = df_nodes[df_nodes["parent"] == node_id]
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
       


##check if parents younger than descendants!!!
#based on a node id
#no need to change
def find_closest_parents_with_date_PD(a):##find closest node parents that have a date
    #based on a node id(a)
    cp = nodes_for_age_function.iat[int(a)-1,1]#current parents 
    while cp != -27400288:#not the first node
        date_estimate = find_age_for_pd_estimate(cp)
        if date_estimate > 0:
            return([cp,date_estimate])#(a list of id and age estimate)
        else:
            cp = nodes_for_age_function.iat[int(cp)-1,1]
            #will return 4025 if no parents have age



#a list of parent
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



df_leaves = pd.read_csv("updated_ordered_leaves_2.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])

df_ed = pd.read_csv("ed_boostrap_new2(100).csv",low_memory=False) 
ed_values = df_ed.drop(columns = ["id","parent","ed","sum_ed","ott","Unnamed: 0"])
#这里

ages = pd.read_csv("latest_node_dates(real_parent)_2.0.csv", low_memory=False)
df_nodes = pd.read_csv("updated_ordered_nodes_2.0.csv",low_memory=False)

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
#print((end_time-start_time)/3600)

#check nodes_for_age_function



##now a nodes table for pd estimate is prepared

##check
nodes_with_pd = df_nodes[['id', 'name', 'ott', 'parent', 'leaf_lft', 'leaf_rgt']]
nodes_with_pd["richness"] = nodes_with_pd["leaf_rgt"]-nodes_with_pd["leaf_lft"]
nodes_with_pd["richness"] = nodes_with_pd.apply(lambda x:x["richness"]+1, axis = 1)



##ccalculate ed for the entire table#
nodes_with_pd["id"] = nodes_with_pd["id"].astype(int)


##now sampling
nodes_with_pd = nodes_with_pd.fillna(0)

##1. sampling
# make a group column

named_pd = nodes_with_pd.query("name != 0")
unnamed_pd = nodes_with_pd.query("name == 0")

named_pd["log_Richness"] = named_pd["richness"].apply(np.log10)
unnamed_pd["log_Richness"] = unnamed_pd["richness"].apply(np.log10)



ls_named_group = []
for row in named_pd.itertuples():
    rich = getattr(row,"log_Richness")
    if 0< rich <= 1:
        ls_named_group.append(1)
    if 1< rich <= 2:
        ls_named_group.append(2)
    if 2< rich <= 3:
        ls_named_group.append(3)
    if 3< rich <= 4:
        ls_named_group.append(4)
    if 4< rich <= 5:
        ls_named_group.append(5)
    if 5< rich <= 6:
        ls_named_group.append(6)
    if 6< rich <= 7:
        ls_named_group.append(7) 
named_pd["Group"] = ls_named_group



ls_unnamed_group = []                
for row in unnamed_pd.itertuples():
    rich = getattr(row,"log_Richness")
    if 0< rich <= 1:
        ls_unnamed_group.append(1)
    if 1< rich <= 2:
        ls_unnamed_group.append(2)
    if 2< rich <= 3:
        ls_unnamed_group.append(3)
    if 3< rich <= 4:
        ls_unnamed_group.append(4)
    if 4< rich <= 5:
        ls_unnamed_group.append(5)
    if 5< rich <= 6:
        ls_unnamed_group.append(6)
    if 6< rich <= 7:
        ls_unnamed_group.append(7)
unnamed_pd["Group"] = ls_unnamed_group



get_realp = df_nodes[["id","real_parent"]]
named_pd = pd.merge(named_pd,get_realp,how = "left",on = "id")
unnamed_pd = pd.merge(unnamed_pd,get_realp,how = "left",on = "id")





lsseerealp = list(set(list(df_nodes["real_parent"])+list(df_leaves["real_parent"])))

lssee_unnamed = []
for row in unnamed_pd.itertuples():
    nodeid = getattr(row,"id")
    if nodeid in lsseerealp:
        lssee_unnamed.append(1)
    else:
        lssee_unnamed.append(0)
    
unnamed_pd["is_realp"] = lssee_unnamed


unnamed_pd = unnamed_pd[unnamed_pd["is_realp"] == 1]

#40631
stratified_sample_named_pd, _ = train_test_split(named_pd, test_size=0.7, stratify=named_pd[['Group']])

#40447
stratified_sample_unnamed_pd, _ = train_test_split(unnamed_pd, test_size=0.39, stratify=unnamed_pd[['Group']])


stratified_sample_named_pd["ED_from_previous_nodes"] = stratified_sample_named_pd["id"].apply(ed_node_realp)##combine real parent

stratified_sample_unnamed_pd["ED_from_previous_nodes"] = stratified_sample_unnamed_pd["id"].apply(ed_node_realp)##combine real parent

stratified_sample_named_pd["misadded_pd"] = stratified_sample_named_pd["ED_from_previous_nodes"] * stratified_sample_named_pd["richness"]
stratified_sample_unnamed_pd["misadded_pd"] = stratified_sample_unnamed_pd["ED_from_previous_nodes"] * stratified_sample_unnamed_pd["richness"]

stratified_sample_named_pd["missing_pd"] = stratified_sample_named_pd["id"].apply(find_missing_PD) 
stratified_sample_unnamed_pd["missing_pd"] = stratified_sample_unnamed_pd["id"].apply(find_missing_PD)


ls_of_pd_list_named = []
for row in stratified_sample_named_pd.itertuples():
    misadded_pd = getattr(row,"misadded_pd")
    missing_pd= getattr(row,"missing_pd")
    lft = getattr(row,"leaf_lft")
    rgt = getattr(row,"leaf_rgt")
    ed_ranged = ed_values[lft-1:rgt]#index = leaf_id - 1
    sum_ed = list(ed_ranged.apply(lambda x: x.sum(), axis = 0))###sum of ed scores
    ##axis = 0:rows
    ls_pd = []
    for i in sum_ed:
        corrected_pd = i - misadded_pd+missing_pd#
        ls_pd.append(corrected_pd)#should be a list of 100 pd scores
    ls_of_pd_list_named.append(ls_pd)


#
ls_of_pd_list_unnamed = []
for row in stratified_sample_unnamed_pd.itertuples():
    misadded_pd = getattr(row,"misadded_pd")
    missing_pd= getattr(row,"missing_pd")
    lft = getattr(row,"leaf_lft")
    rgt = getattr(row,"leaf_rgt")
    ed_ranged = ed_values[lft-1:rgt]#index = leaf_id - 1
    sum_ed = list(ed_ranged.apply(lambda x: x.sum(), axis = 0))###sum of ed scores
    ##axis = 0:rows
    ls_pd = []
    for i in sum_ed:
        corrected_pd = i - misadded_pd+missing_pd#
        ls_pd.append(corrected_pd)#should be a list of 100 pd scores
    ls_of_pd_list_unnamed.append(ls_pd)
    
    
    
stratified_sample_named_pd["pd"] = ls_of_pd_list_named
stratified_sample_unnamed_pd["pd"] = ls_of_pd_list_unnamed




df_named_pd = pd.DataFrame(stratified_sample_named_pd, columns = ["pd"])
df_named_pd["pd"] = df_named_pd["pd"].astype(str)
df_named_pd["pd"] = df_named_pd["pd"].str.replace("[","")
df_named_pd["pd"] = df_named_pd["pd"].str.replace("]","")
ls_pdn = []
for row in df_named_pd.itertuples():
    pd_list = (getattr(row,"pd"))
    ls_pdn.append(pd_list.split(","))
df_named_pd["pd"] = ls_pdn


df_named_pd1 = df_named_pd["pd"].apply(pd.Series, index = list(range(0,100)))

df_named_pd1["median_PD"]=  df_named_pd1.median(axis=1)
stratified_sample_named_pd["median_PD"] = df_named_pd1["median_PD"]


df_unnamed_pd = pd.DataFrame(stratified_sample_unnamed_pd, columns = ["pd"])

df_unnamed_pd["pd"] = df_unnamed_pd["pd"].astype(str)

df_unnamed_pd["pd"] = df_unnamed_pd["pd"].str.replace("[","")
df_unnamed_pd["pd"] = df_unnamed_pd["pd"].str.replace("]","")
ls_pdu = []
for row in df_unnamed_pd.itertuples():
    pd_list = (getattr(row,"pd"))
    ls_pdu.append(pd_list.split(","))
df_unnamed_pd["pd"] = ls_pdu
df_unnamed_pd1 = df_unnamed_pd["pd"].apply(pd.Series, index = list(range(0,100)))
df_unnamed_pd1["median_PD"]=  df_unnamed_pd1.median(axis=1)
stratified_sample_unnamed_pd["median_PD"] = df_unnamed_pd1["median_PD"]
#
stratified_sample_unnamed_pd.to_csv("sampled_unnamed_pd(real_parent_only).csv",encoding = "gbk")
stratified_sample_named_pd.to_csv("sampled_named_pd(real_parent_only).csv",encoding = "gbk")



pd_24 =nodes_with_pd.query("id == 1|id ==60673|id ==63780|id ==63336|id ==122046|id ==172687|id ==543115|id ==805308|id ==972346|id ==1082356|id ==1452621|id ==1595859|id ==1882225|id ==2056499|id ==840050|id ==840051|id ==840165|id ==841421|id ==875861|id ==889827|id ==889596|id ==889849|id ==884549 |id == 899851")



get_range = pd.DataFrame(df_nodes,columns = ["id","leaf_lft","leaf_rgt"])
df_temp = pd.DataFrame(pd_24,columns = ["id","richness"])
df_temp =pd.merge(df_temp,get_range,how = "left",on = "id")

ls_of_average_ed_list = []
for row in df_temp.itertuples():
    rich = getattr(row,"richness")                         
    lft = int(getattr(row,"leaf_lft"))
    rgt = int(getattr(row,"leaf_rgt"))
    ed_ranged = ed_values[lft-1:rgt]   #index = leaf_id - 1
    sum_ed = list(ed_ranged.apply(lambda x: x.sum(), axis = 0))###sum of ed scores
    aver_ed_list = []
    for i in sum_ed:
        average_ed = i/rich
        aver_ed_list.append(average_ed)
    ls_of_average_ed_list.append(aver_ed_list)

df_temp["average_ed"] = ls_of_average_ed_list
df_ave_ed = pd.DataFrame(df_temp,columns = ["average_ed"])

df_ave_ed1 = df_ave_ed["average_ed"].apply(pd.Series, index = list(range(0,100)))
df_id = pd.DataFrame(df_temp,columns = ["id","richness","leaf_lft","leaf_rgt"])
          
ave_ed_final = pd.concat([df_id,df_ave_ed1],axis = 1)
ave_ed_final.to_csv("average_ed_table_24(real_parent).csv",encoding = "gbk")
