import pandas as pd
import os


#read in leaves table and keep necessary columns. 
df_leaves = pd.read_csv("updated_ordered_leaves_2.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])
leaves2 = leaves1
leaves2['id'] = leaves2['id'].astype(str)
leaves2 = leaves2.groupby("parent")["id"].apply(lambda x:x.str.cat(sep = ",")).reset_index()
#nodes for finding parents
df_nodes = pd.read_csv("updated_ordered_nodes_2.0.csv",low_memory=False)
nodes1 = pd.DataFrame(df_nodes,columns = ["id","ott","parent","real_parent","node_rgt","leaf_lft","leaf_rgt","age"])

    #nodes for calculating ED
nodes = pd.DataFrame(nodes1, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","age"])
nodes = nodes.fillna(0)

nodes_with_pd = df_nodes[["id","name","ott","parent","leaf_lft","leaf_rgt"]]
nodes_with_pd["richness"] = nodes_with_pd["leaf_rgt"]-nodes_with_pd["leaf_lft"]+1
ls_realp_leaf = list(df_leaves["real_parent"])
ls_realp_node = list(df_nodes["real_parent"])
ls_realp =  list(set(ls_realp_leaf+ls_realp_node))
nodes_with_pd = nodes_with_pd[nodes_with_pd['id'].isin(ls_realp)]
nodes_with_pd = nodes_with_pd[["id","name","ott","richness"]]
new_index = [int(i) for i in range(0, len(nodes_with_pd.index))]
nodes_with_pd.index = new_index




def generate_rows_to_read(total_rows, start, step=3):
    return list(range(start, total_rows, step))

def read_specific_rows_incrementally(file_path, rows_to_read):
    ed_values_df = []
    
    with open(file_path, 'r') as file:
        for i, line in enumerate(file):
            if i in rows_to_read:  
                ed_values_df.append(line.strip().split(','))  
    

    ed_values_df = pd.DataFrame(ed_values_df)
    return ed_values_df



file_path = " jackknife _all.csv"#merge all *.part genetated by HPC





def read_dataframe(file_path,type_of_data):

    with open(file_path, 'r') as file:
        total_rows = sum(1 for _ in file)  
    if type_of_data == "pd":
        rows_to_read = generate_rows_to_read(total_rows,2, step=3)#rows_pd
        pd_values1 = read_specific_rows_incrementally(file_path, rows_to_read)
        pd_values1.iloc[:, 0] = pd_values1.iloc[:, 0].str.replace('[', '', regex=False)     
        pd_values1.iloc[:, -1] = pd_values1.iloc[:, -1].str.replace(']', '', regex=False)
        pd_values1 = pd_values1.T
        pd_with_id = pd.concat([nodes_with_pd, pd_values1], axis=1)
        return(pd_with_id)
    
    if type_of_data == "ed":
        rows_to_read = generate_rows_to_read(total_rows,0, step=3)#rows_ed
        ed_values1 = read_specific_rows_incrementally(file_path, rows_to_read)
        ed_values1.iloc[:, 0] = ed_values1.iloc[:, 0].str.replace('(', '', regex=False)     
        ed_values1.iloc[:, -1] = ed_values1.iloc[:, -1].str.replace(')', '', regex=False)

        ed_values1 = ed_values1.T
        ed_with_id = pd.concat([ed_values1, leaves2], axis=1)
        ed_with_id = ed_with_id.drop(columns=['id'])
        ed_values_all = pd.merge(leaves1,ed_with_id,how = "left",on = "parent")
        return(ed_values_all)



ed = read_dataframe(file_path,"ed")
ed.to_csv("ed.csv",encoding = "gbk")


pd = read_dataframe(file_path,"pd")
pd.to_csv("pd.csv",encoding = "gbk")


