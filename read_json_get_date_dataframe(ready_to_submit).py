


import pandas as pd
import numpy as np
import os
import re

os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")#"/Users/alexgjl/Desktop/master/project2/field"

###read into the json field and change it into a dataframe


ages = pd.read_json("updated_node_ages.json")##read into the latest json field

df_ages = pd.DataFrame(ages)
df_ages['node_ages'] = df_ages['node_ages'].astype(str)


#1.deal with date for 2 nodes
df_ages1 = df_ages.loc[df_ages.index.str.contains("mrcaott")]

listage = []
for row in df_ages1.itertuples():
    listage.append(row)
    
df_ages_2ott = pd.DataFrame(listage)

df_ages_2ott['mrca'] = df_ages_2ott["Index"].map(lambda x:x.split("ott")[0])
df_ages_2ott['ott1'] = df_ages_2ott["Index"].map(lambda x:x.split("ott")[1])
df_ages_2ott['ott2'] = df_ages_2ott["Index"].map(lambda x:x.split("ott")[2])

all_ages_list = []    #list that store the dict of ages

age_list0 = df_ages_2ott['node_ages'].tolist()
for i in age_list0:
    pattern = re.compile(r'\'age\': (\d+\.?\d*),')
    result = pattern.findall(i)
    result = list(map(lambda x:float(x), result))
    all_ages_list.append(result)

df_ages_2ott['ages'] = all_ages_list

df_ages_2ott = pd.DataFrame(df_ages_2ott, columns = ["ott1","ott2","ages"])



#find node_id for the 2 ott

df_nodes = pd.read_csv("ordered_nodes.csv",low_memory=False)##read into the latest node dates
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

df_leaves = pd.read_csv("ordered_leaves.csv",low_memory=False)##read into the latest nodes
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])

agetable1 = df_ages_2ott

def find_leaf_parents(a):##find parents based on a leaf id(a)
    list_parentsl = []
    nodes0 = nodes1.loc[(nodes1["leaf_lft"]<= int(a)) & (nodes1["leaf_rgt"] >= int(a))]
    for row in nodes0.itertuples():
        list_parentsl.append(getattr(row,"id"))
    return list_parentsl


def find_node_parents(a):##find parents based on a node id(a)
    list_parentsn = []
    lft = nodes1.iat[int(a)-1,5]
    rgt = nodes1.iat[int(a)-1,6]
    nodesn = nodes1.loc[(nodes1["leaf_lft"]<= lft) & (nodes1["leaf_rgt"] >= rgt)]
    for row in nodesn.itertuples():
        list_parentsn.append(getattr(row,"id"))
    return list_parentsn


def find_leaf_parents_1(a):##find parents based on a leaf id(a)
    list_parentsl = []
    cp = leaves1.iat[int(a)-1,1]
    while cp != -27400288:
        list_parentsl.append(cp)
        cp = nodes1.iat[int(cp)-1,2]
    return list_parentsl


def find_node_parents_1(a):##find parents based on a node id(a)
    list_parentsn = []
    cp = nodes1.iat[int(a)-1,2]#current parents
    while cp != -27400288:
        list_parentsn.append(cp)
        cp = nodes1.iat[int(cp)-1,2]
    return list_parentsn##whether it should include itself?


def OTT2ID(a):##to see whether this ott id is in leaves table or nodes table
    dict1 = {}
    if a in nodes1["ott"].values:
        nodeid = nodes1.loc[nodes1["ott"]==a, "id"].iloc[0]
        dict1["nodeid"] = nodeid
        return(dict1)
    if a in leaves1["ott"].values:
        leafid = leaves1.loc[leaves1["ott"]==a, "id"].iloc[0]
        dict1["leafid"] = leafid
        return(dict1)
    else:
        return(-1)
   
####only case 2 need to be solve: ages with 2 ott id
def find_commonancestor(a,b):## a and b are ott id
    id1 = OTT2ID(a)##a dictionary
    id2 = OTT2ID(b)
    ##for ott a
    if OTT2ID(a) == -1 or OTT2ID(b) == -1:
        return(-1)###neither in leaves nor nodes
    else:
        if list(id1)[0] == "nodeid":
            nodeida = list(id1.values())[0]
            parentsa = find_node_parents_1(nodeida)###list of parents
        if list(id1)[0] == "leafid":
            leafida = list(id1.values())[0]
            parentsa = find_leaf_parents_1(leafida)###list of parents
        ###for ott b
        if list(id2)[0] == "nodeid":
            nodeidb = list(id2.values())[0]
            parentsb = find_node_parents_1(nodeidb)###list of parents
                        ###find list of parents from node table
        if list(id2)[0] == "leafid":
            leafidb = list(id2.values())[0]
            parentsb = find_leaf_parents_1(leafidb)
        parentsa = sorted(parentsa,reverse = True)
        #print(parentsa[0:10])
        parentsb = sorted(parentsb,reverse = True)
        #print(parentsb[0:10])
        for i in parentsa:
            if i in parentsb:
                nodeid = i
                break
        return(nodeid)

ls_nodeid = []  
for row in agetable1.itertuples():
    ott1 = int(getattr(row,"ott1"))
    ott2 = int(getattr(row,"ott2"))
    ls_nodeid.append(find_commonancestor(ott1,ott2))
agetable1["id"] = ls_nodeid##a table of age of 2 ott with date estimates


#Now it's time to deal with nodes that have only 1 ott
df_ages2 = df_ages[~df_ages.index.str.contains("mrcaott")]#select dates have only 1 ott

listage1 = []
for row in df_ages2.itertuples():
    listage1.append(row)
    
df_ages_1ott = pd.DataFrame(listage1)
df_ages_1ott['ott'] = df_ages_1ott["Index"].map(lambda x:x.split("ott")[1])



all_ages_list2 = []    #list that store the dict of ages

age_list1 = df_ages_1ott['node_ages'].tolist()
for i in age_list1:
    pattern = re.compile(r'\'age\': (\d+\.?\d*),')
    result = pattern.findall(i)
    result = list(map(lambda x:float(x), result))
    all_ages_list2.append(result)

df_ages_1ott['ages'] = all_ages_list2
df_ages_1ott = pd.DataFrame(df_ages_1ott, columns = ["ott","ages"])

##merge this date table with nodes1 to get node id

df_ages_1ott['ott'] = df_ages_1ott['ott'].astype(float)
df_ages_1ott['ott'] = df_ages_1ott['ott'].astype(str)




nodes1['ott'] = nodes1['ott'].astype(str)
nodes2 = pd.DataFrame(nodes1,columns = ["id","ott"])
agetable2 = pd.merge(df_ages_1ott, nodes2, how = "left",on = "ott")

agetable1 = pd.DataFrame(agetable1,columns = ["id", "ages"])

agetable2 = pd.DataFrame(agetable2,columns = ["id", "ages"])
agetable2 = agetable2.loc[agetable2["id"] > 0]##some nodes are not in nodes table, therefore do not have id

##find nodes that have age in ordered_nodes_table
agetable3 = nodes1.loc[nodes1["age"] > 0]
agetable3 = pd.DataFrame(agetable3,columns = ["ott","id","age"])
agetable3["ott"] = agetable3["ott"].astype(float)
agetable3 = agetable3.drop(agetable3[agetable3["ott"] > 0].index)
agetable3["ott"] = agetable3["ott"].astype(str)
agetable3["ages"] = agetable3["age"]
agetable3 = pd.DataFrame(agetable3,columns = ["id","ages"])



agetable = pd.concat([agetable1,agetable2,agetable3])
agetable['ages'] = agetable['ages'].astype(str)
##remove “[]" in agetable 1 and 2
agetable["ages"] = agetable["ages"].str.replace("[","")
agetable["ages"] = agetable["ages"].str.replace("]","")
agetable = agetable.loc[agetable["id"] > 0]

##deal with overlapped node id:
agetable = agetable.groupby("id")["ages"].apply(lambda x:x.str.cat(sep = ",")).reset_index()

agetable.to_csv("latest_node_dates.csv",encoding = "gbk")


