import pandas as pd
import os


#read in leaves table and keep necessary columns. 
df_leaves = pd.read_csv("updated_ordered_leaves_3.0.csv",low_memory=False)
leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott","real_parent"])
leaves2 = leaves1
leaves2['id'] = leaves2['id'].astype(str)
leaves2 = leaves2.groupby("parent")["id"].apply(lambda x:x.str.cat(sep = ",")).reset_index()
#nodes for finding parents
df_nodes = pd.read_csv("updated_ordered_nodes_3.0.csv",low_memory=False)
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



vals = pd.read_csv("/rds/general/user/jg621/home/ed_bootstrap/edpd.csv",low_memory=False)
file_path = "/rds/general/user/jg621/home/ed_bootstrap/edpd.csv"#merge all *.part genetated by HPC





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



def read_dataframe(file_path,type_of_data):
    with open(file_path, 'r') as file:
        total_rows = sum(1 for _ in file)  
    if type_of_data == "pd":
        rows_to_read = generate_rows_to_read(total_rows,2, step=4)#rows_pd
        pd_values1 = read_specific_rows_incrementally(file_path, rows_to_read)
        pd_values1.iloc[:, 0] = pd_values1.iloc[:, 0].str.replace('[', '', regex=False)     
        pd_values1.iloc[:, -1] = pd_values1.iloc[:, -1].str.replace(']', '', regex=False)
        pd_values1 = pd_values1.T
        pd_with_id = pd.concat([nodes_with_pd, pd_values1], axis=1)
        return(pd_with_id)
    if type_of_data == "ed":
        rows_to_read = generate_rows_to_read(total_rows,0, step=4)#rows_ed
        ed_values1 = read_specific_rows_incrementally(file_path, rows_to_read)
        ed_values1.iloc[:, 0] = ed_values1.iloc[:, 0].str.replace('[', '', regex=False)     
        ed_values1.iloc[:, -1] = ed_values1.iloc[:, -1].str.replace(']', '', regex=False)
        ed_values1 = ed_values1.T
        ed_with_id = pd.concat([ed_values1, leaves2], axis=1)
        ed_with_id = ed_with_id.drop(columns=['id'])
        ed_values_all = pd.merge(leaves1,ed_with_id,how = "left",on = "parent")
        return(ed_values_all)
    if type_of_data == "pd-rikki":
        rows_to_read = generate_rows_to_read(total_rows,3, step=4)#rows_pd
        pd_values1 = read_specific_rows_incrementally(file_path, rows_to_read)
        pd_values1.iloc[:, 0] = pd_values1.iloc[:, 0].str.replace('[', '', regex=False)     
        pd_values1.iloc[:, -1] = pd_values1.iloc[:, -1].str.replace(']', '', regex=False)
        pd_values1 = pd_values1.T
        pd_with_id = pd.concat([nodes_with_pd, pd_values1], axis=1)
        return(pd_with_id)



ed = read_dataframe(file_path,"ed")

pd_vals = read_dataframe(file_path,"pd")


ed.columns = list(ed.columns[:4]) + [f"ed{i+1}" for i in range(len(ed.columns)-4)]
pd_vals.columns = list(pd_vals.columns[:4]) + [f"pd{i+1}" for i in range(len(pd_vals.columns)-4)]



ed_newvals = ed[[col for col in ed.columns if col.startswith("ed")]]
pd_newvals = pd_vals[[col for col in pd_vals.columns if col.startswith("pd")]]

def fix_columns(df, prefix, expected=1000):
    cols = [col for col in df.columns if col.startswith(prefix)]
    count = len(cols)

    if count == expected:
        print(f"{prefix}: 列数正确 = {count}")
        return df

    elif count > expected:
        print(f"{prefix}: 检测到列数 {count} > {expected}，自动删除多余列")
        extra = sorted(cols)[expected:]
        df = df.drop(columns=extra)
        return df

    else:
        print(f"{prefix}: 列数不足 {count} < {expected}，这说明某些 part 合并损坏！（需要排查）")
        return df



ed_newvals = fix_columns(ed_newvals, "ed", expected=1000)
pd_newvals = fix_columns(pd_newvals, "pd", expected=1000)

ed_newvals = ed_newvals.apply(pd.to_numeric, errors='coerce')
pd_newvals = pd_newvals.apply(pd.to_numeric, errors='coerce')


ed_newvals["median"] = ed_newvals.median(axis=1)
pd_newvals["median"] = pd_newvals.median(axis=1)


ed_value = ed_newvals
pd_value = pd_newvals

ed_value["id"] = ed["id"]
pd_value["id"] = pd_vals["id"]
pd_value["name"] = pd_vals["name"]


ed["median"] =ed_newvals["median"] 
pd_vals["median"] =pd_newvals["median"]

medianed_df = ed[["id","median"]]
medianpd_df = pd_vals[["id","name","median"]]



ed_value.to_csv("ed_values.csv",encoding = "gbk")
pd_value.to_csv("pd_values.csv",encoding = "gbk")
medianed_df.to_csv("ed_median.csv",encoding = "gbk")
medianpd_df.to_csv("pd_median.csv",encoding = "gbk")



