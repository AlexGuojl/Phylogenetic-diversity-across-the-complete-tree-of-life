import pandas as pd
import numpy as np

leaves = pd.read_csv("ordered_leaves.csv",low_memory=False)
df_leaves = pd.DataFrame(leaves)#convert csv to dataframe

nodes = pd.read_csv("ordered_nodes.csv",low_memory=False)
df_nodes = pd.DataFrame(nodes)

#select column(id, parent, real parent,node_rgt,leaf_lft,leaf_rgt)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","parent","real_parent","node_rgt","leaf_lft","leaf_rgt"])
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])

#print(nodes1.iat[1,4])

###tasks

def find_descendants(id):#find number of descendants in node table 
    lf = nodes1.iat[int(id)-1,4]
    lr = nodes1.iat[int(id)-1,5]
    num_des = lr-lf+1
    return num_des

##open tree id 1 , open tree id 2 -> node id
def find_commonancestors(a,b):
    #the row numbers of the ott pair
    list1 = []
    num_row1 = leaves1[leaves1.ott == a].index.tolist()[0]
    num_row2 = leaves1[leaves1.ott == b].index.tolist()[0]
    #get the leaves id
    leaf_id1 = leaves1.iat[num_row1,0]
    list1.append(leaf_id1)
    leaf_id2 = leaves1.iat[num_row2,0]
    list1.append(leaf_id2)
    list1 = sorted(list1)     #decide leaf left(list1[0]) and leaf righr(list1[1])
    node_id  = 1
    dict_nodeid = {}
    for row in nodes1.itertuples():
        if int(getattr(row,"leaf_lft")) <= int(list1[0]) and int(getattr(row,"leaf_rgt")) >= int(list1[1]):
            length = int(getattr(row,"leaf_rgt")) - int(getattr(row,"leaf_lft"))
            node_id = (getattr(row,"id"))
            dict_nodeid[node_id] = length#以node_id——key，length——value
    nodeid1 = sorted(dict_nodeid.items(),key = lambda x:x[1])
    c = nodeid1[0]
    return(c[0])
#leaf id -> list of all parents

def find_parents(a):
    list_parents = []
    for row in nodes1.itertuples():
        if int(getattr(row,"leaf_lft")) <= int(a) and int(getattr(row,"leaf_rgt")) >= int(a):
            list_parents.append(getattr(row,"id"))
    return list_parents

### reassemble the json field

ages = pd.read_json("node_ages.json")
df_ages = pd.DataFrame(ages)

#a dict in list for each pair of ottid

##keep the rows that have 2 ott id
df_ages['node_ages'] = df_ages['node_ages'].astype(str)



#ages0 = df_ages.loc[df_ages["node_ages"].str.contains("mrcaottmrcaott")]
ages0 = df_ages.drop(df_ages.tail(8001).index)
listage = []
for row in ages0.itertuples():
    listage.append(row)

ages_final = pd.DataFrame(listage)


ages_final['mrca'] = ages_final["Index"].map(lambda x:x.split("ott")[0])
ages_final['ott1'] = ages_final["Index"].map(lambda x:x.split("ott")[1])
ages_final['ott2'] = ages_final["Index"].map(lambda x:x.split("ott")[2])

#one common ancestor for every row

#######################################

###ages_final is a field of leaves

recent_ancestor = pd.read_csv("list_commonancestor.csv",low_memory=False)
df_recent_ancestor = pd.DataFrame(recent_ancestor)#convert csv to dataframe

############################ combine the dataframe #############################

df_nodes_with_age = pd.concat([ages_final,df_recent_ancestor],axis = 1)###common ancestor node id


##add a new column of all the descendants

nodes1["num_descendants"] = nodes1["leaf_rgt"]-nodes1["leaf_lft"]+1


###write the age into node table，merge

df_nodes_with_age["id"] = df_nodes_with_age["most_recent_ancestor"]

mergenodes = pd.DataFrame(df_nodes_with_age,columns = ["id","node_ages"])

#以nodeid为标准通过merge合并，获取"node_ages"

#mergenodes.set_index(["id"],inplace = True)_set index for join

#nodes1.set_index(["id"],inplace = True)

#nodes_final = nodes1.join(mergenodes)
##delete the node have no age
nodes2 = pd.merge(nodes1,mergenodes)#####sorted nodes with age

#nodes2[0:5].to_csv("nodes_check.csv",encoding = "gbk")
#list of several dics

import re

all_ages_list = []#list that store the dict of ages
age_list0 = nodes2['node_ages'].tolist()
for i in age_list0:
    pattern = re.compile(r'{\'age\': (\d+\.?\d*),')
    result = pattern.findall(i)
    result = list(map(lambda x:float(x), result))
    all_ages_list.append(result)

nodes2['ages'] = all_ages_list

#先把common ancestor和ages合并

#combine all_ages_list and df_list_commonancestor






#define a function for calculating the ED value

#collect ages for the node_ages column

