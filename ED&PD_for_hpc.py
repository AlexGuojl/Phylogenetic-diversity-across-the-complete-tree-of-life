# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
import os
import random
from random import choice
import sys
from multiprocessing import Pool, set_start_method
import json
# ---------get randomseed-----------
arg = int(sys.argv[1])  # receive parameter

# ---------- read in files ----------
df_leaves = pd.read_csv("updated_ordered_leaves_3.0.csv", low_memory=False)
leaves1 = pd.DataFrame(df_leaves, columns=["id", "parent", "ott", "real_parent"])

df_nodes = pd.read_csv("updated_ordered_nodes_3.0.csv", low_memory=False)
# nodes1 in original code; keep df_nodes as source of truth
nodes1 = pd.DataFrame(df_nodes, columns=["id", "ott", "parent", "real_parent", "node_rgt", "leaf_lft", "leaf_rgt", "age"])
nodes = pd.DataFrame(nodes1, columns=["Unnamed: 0", "id", "parent", "leaf_lft", "leaf_rgt", "unnamed:0", "age"])
nodes = nodes.fillna(0)

# build leaves2 exactly as your original
leaves2 = leaves1.copy()
leaves2['id'] = leaves2['id'].astype(str)
leaves2 = leaves2.groupby("parent")["id"].apply(lambda x: x.str.cat(sep=",")).reset_index()

new_id_ls = []
for row in leaves2.itertuples():
    leaf_id = getattr(row, "id")
    if "," in leaf_id:
        ls_leaf_id = leaf_id.split(",")
        leaf_id = int(ls_leaf_id[0])
        new_id_ls.append(leaf_id)
    else:
        leaf_id = int(leaf_id)
        new_id_ls.append(leaf_id)

leaves2["new_id"] = new_id_ls

# ---------- ages bootstrap (preserve exactly your logic) ----------
nodes_no_age = pd.DataFrame(nodes, columns=["Unnamed: 0", "id", "parent", "leaf_lft", "leaf_rgt", "unnamed:0"])
ages = pd.read_csv("latest_node_dates(real_parent)_3.0.csv", low_memory=False)

random.seed(arg)
ages_selected = ages.sample(frac=0.5, replace=False, random_state=arg)

ls_age = []
for row in ages_selected.itertuples():
    age = getattr(row, "ages")
    if "," in age:
        ls_ages = age.split(",")
        age_selected = choice(ls_ages)
        ls_age.append(age_selected)
    else:
        ls_age.append(age)

ages_selected['age'] = ls_age
ages_selected["age"] = pd.to_numeric(ages_selected["age"])
ages_selected = pd.DataFrame(ages_selected, columns=['id', 'age'])
ages_bootstrap_final = pd.merge(nodes_no_age, ages_selected, how="left", on="id")
ages_bootstrap_final = ages_bootstrap_final.fillna(0)
ages_bootstrap_final.iat[0, 6] = 4246.666667
ages_bootstrap_final["age"] = pd.to_numeric(ages_bootstrap_final["age"])


def find_age(a):  # will be used by other functions
    if a == -1:
        return 0
    else:
        return ages_bootstrap_final.iloc[a-1, 6]

def find_parents_with_date(node_id):
    list_try = []
    cp = df_nodes.iat[int(node_id)-1, 2]
    while cp != -27400288:
        list_try.append(cp)
        cp = df_nodes.iat[int(cp)-1, 2]
    list_try.append(node_id)
    list_try = sorted(list_try, reverse=True)
    list_date = [find_age(i) for i in list_try]
    return [i for i in list_date if i > 0]

for row in ages_bootstrap_final[ages_bootstrap_final["age"] > 0][1:].itertuples():
    node_id = int(getattr(row, "id"))
    lsdates = find_parents_with_date(node_id)
    lsdates1 = sorted(lsdates)
    if lsdates == lsdates1:
        continue
    else:
        i = 0
        ls_chose = []
        while i < len(lsdates):
            for j in range(1 + i, len(lsdates)):
                if lsdates[i] <= lsdates[j]:
                    continue
                if lsdates[i] > lsdates[j]:
                    ls_chose.append(lsdates[i])
                    ls_chose.append(lsdates[j])
                    selected = choice(ls_chose)
                    if selected == lsdates[j]:
                        inds = ages_bootstrap_final[ages_bootstrap_final.age == selected].index.tolist()
                        for k in inds:
                            ages_bootstrap_final.at[k, "age"] = 0
                        ls_chose = []
                    if selected == lsdates[i]:
                        inds = ages_bootstrap_final[ages_bootstrap_final.age == selected].index.tolist()
                        for k in inds:
                            ages_bootstrap_final.at[k, "age"] = 0
                        ls_chose = []
            i += 1


def find_des(a):
    if a == -1:
        return 1
    else:
        return 1 / (nodes.iloc[a-1, 4] - nodes.iloc[a-1, 3] + 1)

# ed_new3 original logic (unchanged semantically)
def ed_new3(leaf_id):
    list_try = []
    ls_bls = []
    cp = leaves1.iat[int(leaf_id)-1, 1]
    ## find leaf parents
    while cp != -27400288:
        list_try.append(cp)
        cp = nodes.iat[int(cp)-1, 2]
    list_try = sorted(list_try, reverse=True)
    ls_des = []
    ls_part = []
    ls_ed = []
    ind = 1
    list_try.insert(0, -1)
    while ind < len(list_try):
        if find_age(list_try[ind]) == 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ind += 1
        if find_age(list_try[ind]) > 0:
            ls_part.append(list_try[ind])
            ls_des.append(find_des(list_try[ind-1]))
            ed_temp = ((find_age(list_try[ind]) - find_age(list_try[ind-len(ls_part)])) / len(ls_part)) * sum(ls_des)
            branch_len = (find_age(list_try[ind]) - find_age(list_try[ind-len(ls_part)])) / len(ls_part)
            ls_bls.append(branch_len)
            ls_ed.append(ed_temp)
            ind += 1
            ls_part = []
            ls_des = []
    # guard: if no ls_bls found, set terminal branch len to 0
    term_bls = ls_bls[0] if len(ls_bls) > 0 else 0.0
    return float(format(sum(ls_ed), '.6f')), term_bls

# ---------- PD-supporting functions (preserve exactly) ----------
def find_age_for_pd_estimate(a):  # use the nodes_with_age table, "a" refers to a node id
    if a == -1:
        return 0
    if float(nodes_for_age_function.iloc[a-1, 2]) > 0:
        return float(nodes_for_age_function.iloc[a-1, 2])
    else:
        return 0

def find_missing_PD(node_id):
    if node_id == 1:
        return 0
    node_id_local = nodes_for_age_function.iat[int(node_id)-1, 1]  # exclude the unique short branch
    if find_age_for_pd_estimate(node_id_local) > 0:
        return float(4246.666667 - find_age_for_pd_estimate(node_id_local))
    else:
        df_des_node = df_nodes[df_nodes["parent"] == node_id_local]
        ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
        ls_des_age = sorted(ls_des_age, reverse=True)
        count_up = 0
        count_down = 0
        if ls_des_age == []:
            ls_des_age.append(0)
            count_down += 1
        if max(ls_des_age) > 0:
            count_down += 1
        else:
            while True:
                df_des_node = df_nodes[df_nodes["parent"].isin(list(df_des_node["id"]))]
                ls_des_age = df_des_node["id"].apply(find_age_for_pd_estimate)
                ls_des_age = sorted(ls_des_age, reverse=True)
                count_down += 1

                if ls_des_age == []:
                    ls_des_age.append(0)
                    count_down += 1
                    break
                if max(ls_des_age) == 0:
                    count_down += 1
                    continue
                if max(ls_des_age) > 0:
                    count_down += 1
                    break
    parent_for_node_id = nodes_for_age_function.iat[int(node_id_local)-1, 1]
    while True:
        if find_age_for_pd_estimate(parent_for_node_id) > 0:
            count_up += 1
            BL = (find_age_for_pd_estimate(parent_for_node_id) - max(ls_des_age)) / (count_up + count_down)
            node_date = find_age_for_pd_estimate(parent_for_node_id) - BL * count_up
            break
        else:
            count_up += 1
            parent_for_node_id = nodes_for_age_function.iat[int(parent_for_node_id)-1, 1]
            continue
    return (4246.666667 - node_date)

def ed_node_realp(node_id):  # all the id are nodes, so no need to use leaf_realparent
    if node_id == 1:
        return 0
    node_id_local = df_nodes.iat[int(node_id)-1, 2]
    if find_age_for_pd_estimate(node_id_local) > 0:
        list_try = []
        cp = df_nodes.iat[int(node_id_local)-1, 2]
        while cp != -27400288:
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1, 2]
        list_try.append(node_id_local)
        list_try = sorted(list_try, reverse=True)
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
                ed_temp = ((find_age_for_pd_estimate(list_try[ind]) - find_age_for_pd_estimate(list_try[ind-len(ls_part)])) / len(ls_part)) * sum(ls_des)
                ls_ed.append(ed_temp)
                ind += 1
                ls_part = []
                ls_des = []
        return float(format(sum(ls_ed), '.6f'))

    if find_age_for_pd_estimate(node_id_local) == 0:
        list_try = []
        cp = df_nodes.iat[int(node_id_local)-1, 2]
        while cp != -27400288:
            list_try.append(cp)
            cp = df_nodes.iat[int(cp)-1, 2]
        list_try.append(node_id_local)
        list_try = sorted(list_try, reverse=True)
        ls_des = []
        ls_part = []
        ls_ed = []
        ind = 1
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
                    ed_temp = ((find_age_for_pd_estimate(list_try[ind]) - (4246.666667 - find_missing_PD(node_id_local))) / len(ls_part)) * sum(ls_des)
                    ls_ed.append(ed_temp)
                    ind += 1
                    times += 1
                    ls_part = []
                    ls_des = []
                else:
                    ls_part.append(list_try[ind])
                    ls_des.append(find_des(list_try[ind-1]))
                    ed_temp = ((find_age_for_pd_estimate(list_try[ind]) - find_age_for_pd_estimate(list_try[ind-len(ls_part)])) / len(ls_part)) * sum(ls_des)
                    ls_ed.append(ed_temp)
                    ind += 1
                    ls_part = []
                    ls_des = []
        return float(format(sum(ls_ed), '.6f'))



###############read in Rikki's estimation#####################################
    ############################should include 2 fields: id and ed############
df_rikki_ed = pd.read_csv("Rikki_estimated_ED_with_ID.csv", low_memory=False)
###["ED","id","name"]
#查看数据类型！！！


# ---------- multiprocess ----------
def get_n_cores(default=12):
    try:
        val = os.environ.get("PBS_NP") or os.environ.get("PBS_NCPUS") or os.environ.get("SLURM_CPUS_ON_NODE")
        if val:
            return int(val)
    except Exception:
        pass
    return default

if __name__ == "__main__":#这里代表主程序，防止多线程结束后进入前面的循环
    # use fork to let children inherit parent's memory pages (copy-on-write)
    try:
        set_start_method('fork')#避免将大文件复制给每一个子进程
    except RuntimeError:
        # already set
        pass
    n_cores = get_n_cores(default=12)
    #print("Using cores:", n_cores)

    # --- Parallel ED calculation (low-memory using imap + chunksize) ---
    ls_ed = []
    ls_ed_term = []
    leaf_iterable = (int(x) for x in leaves2["new_id"])  # generator of ints

    with Pool(processes=n_cores) as pool:
        #chunksize tuned for coarser-grained tasks; adjust if needed
        for ed_val, term_val in pool.imap(ed_new3, leaf_iterable, chunksize=500):#对leaf_iterable中对int使用(ed_new3，把id切成500块，imap保证顺序不乱
            ls_ed.append(ed_val)
            ls_ed_term.append(term_val)

    print(ls_ed)
    print(ls_ed_term)
    # build ed dataframe as original
    
    df_ed = pd.DataFrame(ls_ed, columns=["ed"])
    ed_with_id = pd.concat([df_ed, leaves2.reset_index(drop=True)], axis=1)
    ed_values_all = pd.merge(leaves1, ed_with_id, how="left", left_on="parent", right_on="parent")
    ed_values_all["id"] = ed_values_all["id_x"]
    ed_values = pd.DataFrame(ed_values_all, columns=["ed"])
    ############################should include 2 fields: id and ed############
    ##ed_values_all include ed and name/id####################################
    ##replace those ed scores which has been estimated by Rikki###############
    ###########ed_values_rikki = xxxxx
    rikki_map = dict(zip(df_rikki_ed["id"], df_rikki_ed["ED"]))
    #print("dict_generated")
    # Step 2: use map to find replacement where possible
    mapped = ed_values_all["id"].map(rikki_map)
    #print("ed_values_replaced1")
    # Step 3: fallback to original ED where no replacement
    ed_values_all["ed_values_rikki"] = mapped.fillna(ed_values_all["ed"])
    #print("ed_values_replaced2")
    ed_values_rikki = pd.DataFrame(ed_values_all, columns=["ed_values_rikki"])
    #print("ed_values_rikki generated")
    # --- Prepare PD support structures (keep original variables) ---
    nodes_for_age_function = pd.DataFrame(ages_bootstrap_final, columns=["id", "parent", "age"])
    # --- Parallel PD calculation on nodes_with_pd ---
    nodes_with_pd = df_nodes[["id", "name", "ott", "parent", "leaf_lft", "leaf_rgt"]].copy()
    nodes_with_pd["richness"] = nodes_with_pd["leaf_rgt"] - nodes_with_pd["leaf_lft"] + 1
    ls_realp_leaf = list(df_leaves["real_parent"]) if "real_parent" in df_leaves.columns else []
    ls_realp_node = list(df_nodes["real_parent"]) if "real_parent" in df_nodes.columns else []
    ls_realp = list(set(ls_realp_leaf + ls_realp_node))
    nodes_with_pd = nodes_with_pd[nodes_with_pd['id'].isin(ls_realp)].reset_index(drop=True)
    #print("node table generated")
    node_iterable = (int(x) for x in nodes_with_pd["id"])
    ed_node_results = []
    with Pool(processes=n_cores) as pool:
        for val in pool.imap(ed_node_realp, node_iterable, chunksize=50):
            ed_node_results.append(val)
    #print("ed node estimated")
    nodes_with_pd["ED_from_previous_nodes"] = ed_node_results
    nodes_with_pd["misadded_pd"] = nodes_with_pd["ED_from_previous_nodes"] * nodes_with_pd["richness"]
    
    # --- Final PD aggregation (unchanged) ---
    ls_pd = []
    ls_pd2 = []
    
    for row in nodes_with_pd.itertuples():
        misadded_pd = getattr(row, "misadded_pd")
        lft = getattr(row, "leaf_lft")
        rgt = getattr(row, "leaf_rgt")
        ed_ranged = ed_values[lft-1:rgt]
        ed_ranged = list(ed_ranged["ed"])
        sum_ed = sum(ed_ranged)
        ed_rikki_ranged = ed_values_rikki[lft-1:rgt]
        ed_rikki_ranged = list(ed_rikki_ranged["ed_values_rikki"])
        sum_ed_rikki = sum(ed_rikki_ranged)
        corrected_pd = sum_ed - misadded_pd
        corrected_pd2 = sum_ed_rikki - misadded_pd
        ls_pd.append(corrected_pd)
        #print("ls pd estimated")
        ls_pd2.append(corrected_pd2)
        #print("ls pd2 estimated")

    # Optionally save outputs (uncomment if you want files)
    # out_prefix = f"EDPD_result_arg_{arg}"
    # pd.DataFrame(ls_ed, columns=["ed"]).to_csv(out_prefix + "_ed_values.csv", index=False)
    # pd.DataFrame(ls_pd, columns=["pd"]).to_csv(out_prefix + "_pd_values.csv", index=False)
    print(ls_pd)
    print(ls_pd2)    
   # out_prefix = f"{arg}"  # 对应 shell 中的 ${arg_value}.part
    #with open(f"{out_prefix}.part", "w") as f:
     #   json.dump({
      #      "ls_ed": ls_ed,
       #     "ls_ed_term": ls_ed_term,
        #    "ls_pd": ls_pd,
         #   "ls_pd2": ls_pd2
       # }, f)
