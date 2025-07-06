#threatened pd for certain clades(jawed vertebrates and tetrapods)


import pandas as pd
import numpy as np
import os
import random
from random import choice
import time




df_leaves = pd.read_csv("updated_ordered_leaves_2.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])
df_nodes = pd.read_csv("updated_ordered_nodes_2.0.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)

ed_vals = pd.read_csv("ed_values.csv",low_memory=False)
ed_values  = ed_vals.drop(ed_vals.columns[0], axis=1)
#ed_values = ed_values.drop(columns = ["id","parent","ed","sum_ed","ott","Unnamed: 0"])
def find_des(a): ##return 1/descendants for a node id in the whole table
    if a == -1:
        return(1)
    else:
        return(1/(df_nodes.iloc[a-1,6]-df_nodes.iloc[a-1,5]+1))

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


 ##only to nodes that directly connect to leaves!
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
        df_des_node = df_nodes[df_nodes["parent"] == node_id]#This is the first generation of nodes with node_id as parent
        #The purpose of this step is to find a close node date in order to estimate the branch length.
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



def threatened_pd(node_id,dataframe):
    ls_pdnode = []
    #all descendants of this node
    ls_alldes = find_all_descendants_node(node_id)
    #Determine whether this list is a subset of list(df_parent["id"])_#If it is:
    if set(ls_alldes).issubset(set(list(dataframe["id"]))):
     #find node parent
        node_parent = find_closest_parent_node(node_id)
        ls_alldes2 = find_all_descendants_node(node_parent)
    #Get all its descendants through list(range()) and determine whether this list is a subset of threatened_leaves
        if set(ls_alldes2).issubset(set(list(dataframe["id"]))):
            ls_pdnode.append(0)
            return(sum(ls_pdnode))
        else:
            lft = nodes1.iloc[node_id-1,5]
            rgt = nodes1.iloc[node_id-1,6]
            threatened_pd = sum_ed_clade(node_id) - (ed_node_realp(node_id)*(rgt-lft+1))+interial_branch_length(node_id)
            ls_pdnode.append(threatened_pd)
            return(sum(ls_pdnode))
    #if not
    else:
         #threatened pd of this node is the Terminal Branch Length 
        ls_pdnode.append(node_terminal(node_id))
        return(sum(ls_pdnode))


    
getparent = pd.DataFrame(leaves1,columns = ["id","parent","real_parent"])




##has been updated with the latest json data
nodes_no_age = pd.DataFrame(nodes,columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0"])

##always been updated with the latest json data
ages = pd.read_csv("latest_node_dates(real_parent)_2.0.csv", low_memory=False)






#start_time = time.time()
#currentDateAndTime = datetime.now()
# print(currentDateAndTime.strftime("%H:%M:%S")) 
ages_selected = ages 
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

####
#select the 24 groups

df_iucn1 = pd.read_csv("all_iucn_ranked_species.csv",low_memory=False)
df_iucn2 = df_iucn1[df_iucn1["status_code"] != "LC"]
all_threatened = df_iucn2[df_iucn2["status_code"] != "NT"]
all_threatened = all_threatened[all_threatened["status_code"] != "DD"]#skip this skip will give threatened PD in a worse situation in which all DD are regarded as threatened
all_threatened = pd.merge(all_threatened,getparent,how = "left",on = "id")
th_biota = all_threatened
th_euk = all_threatened[all_threatened["id"] > 59642]
th_met = all_threatened[all_threatened["id"] >805307]
th_hol = all_threatened[(all_threatened["id"]>543113)&(all_threatened["id"]<804914)]
th_chl = all_threatened[(all_threatened["id"]>122044)&(all_threatened["id"]<541348)]
th_spe = all_threatened[(all_threatened["id"]>172685)&(all_threatened["id"]<541348)]
th_dia = all_threatened[(all_threatened["id"]>63334)&(all_threatened["id"]<541348)]
th_tsa = all_threatened[(all_threatened["id"]>63777)&(all_threatened["id"]<112742)]
th_mol = all_threatened[(all_threatened["id"]>972344)&(all_threatened["id"]<1061704)]
th_che =all_threatened[(all_threatened["id"]>1082354)&(all_threatened["id"]<1175804)]
th_hym = all_threatened[(all_threatened["id"]>1452619)&(all_threatened["id"]<1588532)]
th_dip = all_threatened[(all_threatened["id"]>1882223)&(all_threatened["id"]<2043133)]
th_lep = all_threatened[all_threatened["id"]>2056498]
th_col = all_threatened[(all_threatened["id"]>1595857)&(all_threatened["id"]<1879264)]
th_vert = all_threatened[(all_threatened["id"]>840048)&(all_threatened["id"]<910759)]
th_agn = all_threatened[(all_threatened["id"]>840048)&(all_threatened["id"]<840163)]
th_chon =all_threatened[(all_threatened["id"]>840162)&(all_threatened["id"]<841419)]
th_oste = all_threatened[(all_threatened["id"]>841418)&(all_threatened["id"]<875851)]
th_amph = all_threatened[(all_threatened["id"]>875858)&(all_threatened["id"]<884547)]
th_croc = all_threatened[(all_threatened["id"]>889823)&(all_threatened["id"]<889847)]
th_test = all_threatened[(all_threatened["id"]>889592)&(all_threatened["id"]<889824)]
th_squa = all_threatened[(all_threatened["id"]>899849)&(all_threatened["id"]<910759)]
th_mam =all_threatened[(all_threatened["id"]>884546)&(all_threatened["id"]<889593)]
th_ave = all_threatened[(all_threatened["id"]>889846)&(all_threatened["id"]<899849)]


th_parent_biota = pd.DataFrame({"parent": list(set(list(th_biota["parent"])))})
th_parent_euk = pd.DataFrame({"parent": list(set(list(th_euk["parent"])))})
th_parent_met = pd.DataFrame({"parent": list(set(list(th_met["parent"])))})
th_parent_hol = pd.DataFrame({"parent": list(set(list(th_hol["parent"])))})
th_parent_chl = pd.DataFrame({"parent": list(set(list(th_chl["parent"])))})
th_parent_spe = pd.DataFrame({"parent": list(set(list(th_spe["parent"])))})
th_parent_dia = pd.DataFrame({"parent": list(set(list(th_dia["parent"])))})
th_parent_tsa = pd.DataFrame({"parent": list(set(list(th_tsa["parent"])))})
th_parent_mol = pd.DataFrame({"parent": list(set(list(th_mol["parent"])))})
th_parent_che = pd.DataFrame({"parent": list(set(list(th_che["parent"])))})

th_parent_hym = pd.DataFrame({"parent": list(set(list(th_hym["parent"])))})
th_parent_dip = pd.DataFrame({"parent": list(set(list(th_dip["parent"])))})
th_parent_lep = pd.DataFrame({"parent": list(set(list(th_lep["parent"])))})
th_parent_vert = pd.DataFrame({"parent": list(set(list(th_vert["parent"])))})
th_parent_col = pd.DataFrame({"parent": list(set(list(th_col["parent"])))})
th_parent_agna = pd.DataFrame({"parent": list(set(list(th_agn["parent"])))})
th_parent_chon = pd.DataFrame({"parent": list(set(list(th_chon["parent"])))})
th_parent_oste = pd.DataFrame({"parent": list(set(list(th_oste["parent"])))})
th_parent_amph = pd.DataFrame({"parent": list(set(list(th_amph["parent"])))})
th_parent_croc = pd.DataFrame({"parent": list(set(list(th_croc["parent"])))})
th_parent_test = pd.DataFrame({"parent": list(set(list(th_test["parent"])))})
th_parent_squa = pd.DataFrame({"parent": list(set(list(th_squa["parent"])))})
th_parent_mam = pd.DataFrame({"parent": list(set(list(th_mam["parent"])))})
th_parent_ave = pd.DataFrame({"parent": list(set(list(th_ave["parent"])))})


###

ls_threatened_pd_biota  = th_parent_biota['parent'].apply(lambda x, th_biota: threatened_pd(x,th_biota ), args=(th_biota,))
ls_threatened_pd_euk  = th_parent_euk['parent'].apply(lambda x, th_euk: threatened_pd(x,th_euk ), args=(th_euk,))
ls_threatened_pd_met  = th_parent_met['parent'].apply(lambda x, th_met: threatened_pd(x,th_met ), args=(th_met,))
ls_threatened_pd_hol  = th_parent_hol['parent'].apply(lambda x, th_hol: threatened_pd(x,th_hol ), args=(th_hol,))
ls_threatened_pd_chl = th_parent_chl['parent'].apply(lambda x, th_chl: threatened_pd(x,th_chl ), args=(th_chl,))
ls_threatened_pd_spe  = th_parent_spe['parent'].apply(lambda x, th_spe: threatened_pd(x,th_spe ), args=(th_spe,))
ls_threatened_pd_dia  = th_parent_dia['parent'].apply(lambda x, th_dia: threatened_pd(x,th_dia ), args=(th_dia,))
ls_threatened_pd_tsa  = th_parent_tsa['parent'].apply(lambda x, th_tsa: threatened_pd(x,th_tsa ), args=(th_tsa,))
ls_threatened_pd_mol  = th_parent_mol['parent'].apply(lambda x, th_mol: threatened_pd(x,th_mol ), args=(th_mol,))
ls_threatened_pd_che  = th_parent_che['parent'].apply(lambda x, th_che: threatened_pd(x,th_che ), args=(th_che,))
ls_threatened_pd_hym  = th_parent_hym['parent'].apply(lambda x, th_hym: threatened_pd(x,th_hym ), args=(th_hym,))
ls_threatened_pd_dip  = th_parent_dip['parent'].apply(lambda x, th_dip: threatened_pd(x,th_dip ), args=(th_dip,))
ls_threatened_pd_lep  = th_parent_lep['parent'].apply(lambda x, th_lep: threatened_pd(x,th_lep ), args=(th_lep,))
ls_threatened_pd_col  = th_parent_col['parent'].apply(lambda x, th_col: threatened_pd(x,th_col ), args=(th_col,))
ls_threatened_pd_vert  = th_parent_vert['parent'].apply(lambda x, th_vert: threatened_pd(x,th_vert ), args=(th_vert,))
ls_threatened_pd_agna  = th_parent_agna['parent'].apply(lambda x, th_agn: threatened_pd(x,th_agn ), args=(th_agn,))
ls_threatened_pd_chon  = th_parent_chon['parent'].apply(lambda x, th_chon: threatened_pd(x,th_chon ), args=(th_chon,))
ls_threatened_pd_oste  = th_parent_oste['parent'].apply(lambda x, th_oste: threatened_pd(x,th_oste ), args=(th_oste,))
ls_threatened_pd_amph  = th_parent_amph['parent'].apply(lambda x, th_amph: threatened_pd(x,th_amph ), args=(th_amph,))
ls_threatened_pd_croc  = th_parent_croc['parent'].apply(lambda x, th_croc: threatened_pd(x,th_croc ), args=(th_croc,))
ls_threatened_pd_test  = th_parent_test['parent'].apply(lambda x, th_test: threatened_pd(x,th_test ), args=(th_test,))
ls_threatened_pd_squa  = th_parent_squa['parent'].apply(lambda x, th_squa: threatened_pd(x,th_squa ), args=(th_squa,))
ls_threatened_pd_mam  = th_parent_mam['parent'].apply(lambda x, th_mam: threatened_pd(x,th_mam ), args=(th_mam,))
ls_threatened_pd_ave  = th_parent_ave['parent'].apply(lambda x, th_ave: threatened_pd(x,th_ave ), args=(th_ave,))



ls_bio = []
ls_euk = []
ls_met = []
ls_holo = []
ls_chl = []
ls_spe = []
ls_dia = []
ls_tsa = []
ls_mol = []
ls_che = []
ls_hym = []
ls_dip = []
ls_lep = []
ls_vert = []
ls_col = []
ls_agna = []
ls_chon = []
ls_oste = []
ls_amph = []
ls_croc = []
ls_test = []
ls_squa = []
ls_mam = []
ls_ave = []


ls_bio.append(sum(ls_threatened_pd_biota))
ls_euk.append(sum(ls_threatened_pd_euk))
ls_met.append(sum(ls_threatened_pd_met))
ls_holo.append(sum(ls_threatened_pd_hol))
ls_chl.append(sum(ls_threatened_pd_chl))
ls_spe.append(sum(ls_threatened_pd_spe))
ls_dia.append(sum(ls_threatened_pd_dia))
ls_tsa.append(sum(ls_threatened_pd_tsa))
ls_mol.append(sum(ls_threatened_pd_mol))
ls_che.append(sum(ls_threatened_pd_che))
ls_hym.append(sum(ls_threatened_pd_hym))
ls_dip.append(sum(ls_threatened_pd_dip))
ls_lep.append(sum(ls_threatened_pd_lep))
ls_col.append(sum(ls_threatened_pd_col))
ls_vert.append(sum(ls_threatened_pd_vert))
ls_agna.append(sum(ls_threatened_pd_agna))
ls_chon.append(sum(ls_threatened_pd_chon))
ls_oste.append(sum(ls_threatened_pd_oste))
ls_amph.append(sum(ls_threatened_pd_amph))
ls_croc.append(sum(ls_threatened_pd_croc))
ls_test.append(sum(ls_threatened_pd_test))
ls_squa.append(sum(ls_threatened_pd_squa))
ls_mam.append(sum(ls_threatened_pd_mam))
ls_ave.append(sum(ls_threatened_pd_ave))

df_bio = pd.DataFrame(ls_bio)
df_bio.to_csv("threatenedpd_biota.csv",encoding = "gbk")
df_euk = pd.DataFrame(ls_euk)
df_euk.to_csv("threatenedpd_euk.csv",encoding = "gbk")
df_met = pd.DataFrame(ls_met)
df_met.to_csv("threatenedpd_metazoa.csv",encoding = "gbk")
df_holo = pd.DataFrame(ls_holo)
df_holo.to_csv("threatenedpd_holomycota.csv",encoding = "gbk")
df_chl = pd.DataFrame(ls_chl)
df_chl.to_csv("threatenedpd_chl.csv",encoding = "gbk")
df_spe = pd.DataFrame(ls_spe)
df_spe.to_csv("threatenedpd_spe.csv",encoding = "gbk")
df_dia = pd.DataFrame(ls_dia)
df_dia.to_csv("threatenedpd_dia.csv",encoding = "gbk")
df_tsa = pd.DataFrame(ls_tsa)
df_tsa.to_csv("threatenedpd_tsa.csv",encoding = "gbk")
df_mol = pd.DataFrame(ls_mol)
df_mol.to_csv("threatenedpd_mol.csv",encoding = "gbk")
df_che = pd.DataFrame(ls_che)
df_che.to_csv("threatenedpd_che.csv",encoding = "gbk")
df_hym = pd.DataFrame(ls_hym)
df_hym.to_csv("threatenedpd_hym.csv",encoding = "gbk")
df_dip = pd.DataFrame(ls_dip)
df_dip.to_csv("threatenedpd_dip.csv",encoding = "gbk")
df_lep = pd.DataFrame(ls_lep)
df_lep.to_csv("threatenedpd_lep.csv",encoding = "gbk")
df_col = pd.DataFrame(ls_col)
df_col.to_csv("threatenedpd_col.csv",encoding = "gbk")
df_vert = pd.DataFrame(ls_vert)
df_vert.to_csv("threatenedpd_vert.csv",encoding = "gbk")
df_agna = pd.DataFrame(ls_agna)
df_agna.to_csv("threatenedpd_cyclostomata.csv",encoding = "gbk")
df_chon = pd.DataFrame(ls_chon)
df_chon.to_csv("threatenedpd_shark_ray.csv",encoding = "gbk")
df_oste = pd.DataFrame(ls_oste)
df_oste.to_csv("threatenedpd_bonyfish.csv",encoding = "gbk")
df_amph = pd.DataFrame(ls_amph)
df_amph.to_csv("threatenedpd_amph.csv",encoding = "gbk")
df_croc = pd.DataFrame(ls_croc)
df_croc.to_csv("threatenedpd_croc.csv",encoding = "gbk")
df_test = pd.DataFrame(ls_test)
df_test.to_csv("threatenedpd_test.csv",encoding = "gbk")
df_squa = pd.DataFrame(ls_squa)
df_squa.to_csv("threatenedpd_squa.csv",encoding = "gbk")
df_mam = pd.DataFrame(ls_mam)
df_mam.to_csv("threatenedpd_mam.csv",encoding = "gbk")
df_ave = pd.DataFrame(ls_ave)
df_ave.to_csv("threatenedpd_ave.csv",encoding = "gbk")

