import pandas as pd
import numpy as np
import os
import re

os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")#"/Users/alexgjl/Desktop/master/project2/field"

###read into the json field and change it into a dataframe



#sort the json field
ages = pd.read_json("updated_node_ages.json")##read into the latest json field

df_ages = pd.DataFrame(ages)
df_ages['node_ages'] = df_ages['node_ages'].astype(str)

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



##some nodes has ott, but they are not listed as real parent, this part will solve this problem

df_nodes = pd.read_csv("ordered_nodes.csv",low_memory=False)##read into the latest node dates
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])
df_leaves = pd.read_csv("ordered_leaves.csv",low_memory=False)##read into the latest nodes
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent",])
agetable1 = df_ages_2ott


nodes_with_ott = df_nodes.loc[df_nodes["ott"] > 0]

ls_see_isinrealp = []#this list will store wether this id is a real parent(1) or not(0)
for row in nodes_with_ott.itertuples():
    node_id_see = int(getattr(row,"id"))
    if node_id_see in list(set(list(leaves1["real_parent"])+list(nodes1["real_parent"]))):
        ls_see_isinrealp.append(1)
    else:
        ls_see_isinrealp.append(0)

nodes_with_ott["is_real_parent"] = ls_see_isinrealp

df_missed_real_parent = nodes_with_ott.loc[nodes_with_ott["is_real_parent"] == 0]

#for correct leaves table
ls_missed_realp = list(df_missed_real_parent["id"])
#for correct nodes table 在
ls_missed_realp0 = list(set(ls_missed_realp).intersection((set(list(nodes1["parent"])))))#only for those nodes which are listed as parent, but not real parent




ls_realp_leaves = []
for row in leaves1.itertuples():
    resolved_parent = getattr(row,"parent")
    real_parent = getattr(row,"real_parent")
    if resolved_parent in ls_missed_realp:
        ls_realp_leaves.append(resolved_parent)
    else:
        ls_realp_leaves.append(real_parent)

df_leaves["real_parent"] = ls_realp_leaves#df leaves is updated here

#df_leaves.to_csv("updated_ordered_leaves_2.0.csv")#an extra column "Unnamed: 0" is included


ls_realp_nodes = []
for row in nodes1.itertuples():
    id_n =  getattr(row,"id")
    resolved_parent_n = getattr(row,"parent")
    real_parent_n = getattr(row,"real_parent")
    if (resolved_parent_n not in ls_missed_realp0) and (id_n not in ls_missed_realp0):
        ls_realp_nodes.append(real_parent_n)
  
    if resolved_parent_n in ls_missed_realp0:
        ls_realp_nodes.append(resolved_parent_n)
       
    if id_n in ls_missed_realp0:
        sub_df = nodes1.loc[nodes1["parent"] == id_n]
        ls = list(sub_df["real_parent"])
        ls1 = [i for i in ls if i >0]
        if len(ls) > 0:
            if len(ls1) > 0:
                ls_realp_nodes.append(ls1[0])
               
            if len(ls1) == 0:
                if -ls[0] in list(nodes1["real_parent"]):
                    ls_realp_nodes.append(-ls[0])
                   
                else:
                    ls_realp_nodes.append(ls[0])
        else:
            ls_realp_nodes.append(real_parent_n)


df_nodes["real_parent"] = ls_realp_nodes#df nodes is updated here
#df_nodes.to_csv("updated_ordered_nodes_2.0.csv",encoding = 'gbk')



#the real_parent of df_nodes and df_leaves has been updated

#find new list includs all real parent!!
ls_realp_leaf = list(df_leaves["real_parent"])
ls_realp_node = list(df_nodes["real_parent"])
ls_realp = list(set(ls_realp_leaf+ls_realp_node))



##find alist of parents
#go through the list, if this id is a real parent, keep it
#otherwise, delete it
#update based on updated nodes and leaves..
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent",])

#now, give age estimate to those real parents





def find_real_leaf_parents(a):##find parents based on a leaf id(a)   find_leaf_parents_1(a)
    list_parentsl = []
    cp = leaves1.iat[int(a)-1,3]
    while cp != -27400288:
        list_parentsl.append(cp)
        cp = nodes1.iat[int(cp)-1,2]
    return sorted(list(set(ls_realp) & set(list_parentsl)),reverse = True)


def find_real_node_parents(a):##find parents based on a node id(a)  find_node_parents_1 previously
    list_parentsn = []
    cp = nodes1.iat[int(a)-1,2]#current parents
    while cp != -27400288:
        list_parentsn.append(cp)
        cp = nodes1.iat[int(cp)-1,2]
    return sorted(list(set(ls_realp_node) & set(list_parentsn)),reverse = True)


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
            parentsa = find_real_node_parents(nodeida)###list of parents
        if list(id1)[0] == "leafid":
            leafida = list(id1.values())[0]
            parentsa = find_real_leaf_parents(leafida)###list of parents
        ###for ott b
        if list(id2)[0] == "nodeid":
            nodeidb = list(id2.values())[0]
            parentsb = find_real_node_parents(nodeidb)###list of parents
                        ###find list of parents from node table
        if list(id2)[0] == "leafid":
            leafidb = list(id2.values())[0]
            parentsb = find_real_leaf_parents(leafidb)
        parentsa = sorted(parentsa,reverse = True)
        #print(parentsa[0:10])
        parentsb = sorted(parentsb,reverse = True)
        #print(parentsb[0:10])
        ls_ca = sorted(list(set(parentsa) & set(parentsb)),reverse = True)
        if len(ls_ca) == 0:
            return(-1)
        else:
            return(ls_ca[0])



#deal with ages have 2 ott
ls_id_node = []  
for row in agetable1.itertuples():
    ott1 = int(getattr(row,"ott1"))
    ott2 = int(getattr(row,"ott2"))
    ls_id_node.append(find_commonancestor(ott1,ott2))##
    
agetable1["id"] = ls_id_node##a table of age of 2 ott with date estimates

############################################################################################run above####################################################################################

#deal with nodes that have only 1 ott
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


nodes1['ott'] = nodes1['ott'].astype(float)
nodes1['ott'] = nodes1['ott'].astype(str)
nodes2 = pd.DataFrame(nodes1,columns = ["id","ott"])
agetable2 = pd.merge(df_ages_1ott, nodes2, how = "left",on = "ott")

agetable1 = pd.DataFrame(agetable1,columns = ["id", "ages"])

agetable2 = pd.DataFrame(agetable2,columns = ["id", "ages"])
agetable2 = agetable2.loc[agetable2["id"] > 0]##some nodes are not in nodes table, therefore do not have id

##find nodes that already have age in ordered_nodes_table
agetable3 = nodes1.loc[nodes1["age"] > 0]
agetable3 = pd.DataFrame(agetable3,columns = ["ott","id","age"])
agetable3["ott"] = agetable3["ott"].astype(float)




#agetable2
lsid_2 = agetable2["id"]
lsid_3 = agetable3["id"]
ls_overlap_id = []
for i in lsid_2:
    if int(i) in lsid_3:
        ls_overlap_id.append(int(i))

for index, row in agetable3.iterrows():
    if row['id'] in ls_overlap_id:
        agetable3 = agetable3.drop(index)


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

#agetable.to_csv("latest_node_dates(real_parent)_2.0.csv",encoding = "gbk")
