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

#read in leaves table and keep necessary columns. 

df_leaves = pd.read_csv("updated_ordered_leaves.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])


#leaves table for calculating ed(leaves2 is a table of leaves in which species that have the same most
##recent ancestor are combined)

#leaves2可以用edgetop20进行merge（"edge20_final.csv"）
df_edge20 = pd.read_csv("edge20_final.csv",low_memory=False)

#read in nodes table.
    #nodes for finding parents
df_nodes = pd.read_csv("updated_ordered_nodes.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

    #nodes for calculating ED
nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)



def find_des(a): ##return 1/descendants for a node id in the whole table
    if a == -1:
        return(1)
    else:
        return(1/(nodes.iloc[a-1,4]-nodes.iloc[a-1,3]+1))

def find_age_for_pd_estimate(a):#use the nodes_with_age table, "a" refers to a node id
    if a == -1:
        return(0)
    if float(nodes_for_age_function.iloc[a-1,2]) > 0:
        return(float(nodes_for_age_function.iloc[a-1,2]))#age get from "age" column
    else:
        return(0)

    

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



def df_edgephylo(leaf_id):
    list_try = []#list of parents
    list_age = []
    list_des = []
    ls_seeifreal = []
    cp = leaves1.iat[int(leaf_id)-1,1]
    ##find leaf parents
    while cp != -27400288:##not the root(oldest node)
        list_try.append(cp)
        list_age.append(find_age_for_pd_estimate(cp))
        list_des.append(1/find_des(cp))        
        cp = nodes.iat[int(cp)-1,2]
    if list_try[0] in list(leaves1["real_parent"]):
        ls_seeifreal.append(1)
    if list_try[0] not in list(leaves1["real_parent"]):
        ls_seeifreal.append(0)
    for i in list_try[1:]:
        if i in list(nodes1["real_parent"]):
            ls_seeifreal.append(1)
        if i not in list(nodes1["real_parent"]):
            ls_seeifreal.append(0)
    list_age.insert(0,0)#
    ind = 1
    ls_part = []
    ls_age_final = []
    while ind < len(list_age):
        if list_age[ind] == 0:
            ls_part.append(list_age[ind])
            ind+=1
        if list_age[ind] > 0:
            ls_part.append(list_age[ind])
            dif_age = (list_age[ind]-list_age[ind-len(ls_part)])
            for j in range(1,len(ls_part)+1):
                ls_age_final.append(list_age[ind-len(ls_part)]+j*dif_age/len(ls_part))
            ind += 1
            ls_part = []
    #now, we have ls_age_final,ls_seeifreal,list_des
    lsall = []
    del list_age[0]
    #lsall.append(list_age)
    list_try.append(leaf_id)
    lsall.append(list_try)
    list_age.append(0)
    lsall.append(list_age)
    ls_age_final.append(0)
    lsall.append(ls_age_final)
    ls_seeifreal.append(0)
    lsall.append(ls_seeifreal)
    list_des.append(1)
    lsall.append(list_des)
    df = pd.DataFrame(lsall)
    df_long = df.transpose()
    df_long.columns = ["all_parent","real_age","age","is_real_parent","num_descendants"]
    df_long["id"] = [leaf_id]*len(list_des)
    return(df_long)

           
##now give an average date to those nodes that does not have an date estimate
def ed_node_realp(node_id): 
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
        df_des_node = df_nodes[df_nodes["parent"] == node_id]#这是第一代descendants——以nodeid为parent的nodes，这一步的目的是向下找出一个接近的node date，以便估计branch length
        ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
        ls_des_age = sorted(ls_des_age,reverse = True)#第[0]位应该是最大的node age
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

    




##has been updated with the latest json data
nodes_no_age = pd.DataFrame(nodes,columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0"])

##always been updated with the latest json data
ages = pd.read_csv("latest_node_dates(real_parent_only).csv", low_memory=False)

#merge df_nodes with node ages
nodes_with_age = pd.merge(df_nodes, ages, how = "left",on = "id")
#1. select those nodes that have age(s)
nodes_with_age= nodes_with_age.fillna(0)


start_time = time.time()
currentDateAndTime = datetime.now()
# print(currentDateAndTime.strftime("%H:%M:%S")) 



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









##combine the two species that have the same most recent common ancestor
#leaves table for calculating ed(leaves2 is a table of leaves in which species that have the same real_parent are combined)

    ##ccalculate ed for the entire table###############################


    #get the end time
end_time = time.time()
print((end_time-start_time)/3600)
print(currentDateAndTime.strftime("%H:%M:%S"))



df_edge20 = pd.DataFrame(df_edge20,columns = ["name","EDGE","ott"])
edge20 = pd.DataFrame(df_edge20,columns = ["name","EDGE","ott"])
getid = pd.DataFrame(leaves1,columns = ["id","ott"])
edge20_final = pd.merge(edge20,getid,how = "left",on = "ott")


##a dataframe of ed scores

 

lsdf = []
lsid = list(edge20_final["id"])
for i in lsid:
    lsdf.append(df_edgephylo(i))
df_phylo = pd.concat(lsdf)


ls_group = []
for row in df_phylo.itertuples():    
    realage = getattr(row,"real_age")
    isrealp =  getattr(row,"is_real_parent")
    leaf_id = int(getattr(row,"id"))
    parent_id = int(getattr(row,"all_parent"))          
    if isrealp == 0 and leaf_id == parent_id:
        ls_group.append("D")
    if isrealp == 0 and leaf_id != parent_id:
        ls_group.append("C")
    if isrealp == 1:
        if realage == 0:
            ls_group.append("B")
        else:
            ls_group.append("A")
df_phylo["Group"] = ls_group

df_median_ed = pd.read_csv("median_ed_final.csv",low_memory=False)
df_median_ed = pd.DataFrame(df_median_ed,columns = ["id","ed"])
df_phylo = pd.merge(df_phylo,df_median_ed,how = "left",on = "id")


ls_node_ed = []
for row in df_phylo.itertuples():
    leaf_id = int(getattr(row,"id"))
    Group = getattr(row,"Group")
    leaf_ed = getattr(row,"ed")
    parent_id = int(getattr(row,"all_parent"))
    if Group == "D":
        ls_node_ed.append(leaf_ed)#   
    else:
        ls_node_ed.append(ed_node_realp(parent_id))




df_phylo["node_ED"] = ls_node_ed
df_phylo.to_csv("phyloinfo_for_edge20_with_ed.csv",encoding = "gbk")
