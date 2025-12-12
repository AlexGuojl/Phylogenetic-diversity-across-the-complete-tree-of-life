import numpy as np
import os
import networkx as nx
import matplotlib.pyplot as plt
from collections import Counter
import pandas as pd
from io import StringIO

os.chdir("/Users/alexgjl/Desktop/master/项目2/文件")  # change this to the path that the files existed in your computer

df_nodes = pd.read_csv("updated_ordered_nodes_3.0.csv", low_memory=False)
nodes1 = pd.DataFrame(df_nodes, columns=["id", "ott", "parent", "real_parent", "node_rgt", "leaf_lft", "leaf_rgt", "age"])
nodes = pd.DataFrame(nodes1, columns=["Unnamed: 0", "id", "parent", "leaf_lft", "leaf_rgt", "unnamed:0", "age"])
nodes = nodes.fillna(0)

#randomly sampled clades based on species richness
sampled_nodes = pd.read_csv("sampled_clades.csv", low_memory=False)

ls_realp = list(df_nodes["real_parent"])

def find_parents_ls(node_id):
    list_try = []  # list of parents
    list_try.append(df_nodes.iat[int(node_id)-1, 2])
    cp = df_nodes.iat[int(list_try[0]) - 1, 2]
    while cp != -27400288:  # not the root (oldest node)
        list_try.append(cp)
        cp = df_nodes.iat[int(cp) - 1, 2]
    list_try = sorted(list_try, reverse=True)
    return sorted(list(set(ls_realp) & set(list_try)), reverse=True)

sampled_nodes["ls_parents"] = sampled_nodes["id"].apply(find_parents_ls)

df = sampled_nodes
for idx, row in df.iterrows():
    df.at[idx, 'ls_parents'].insert(0, row['id'])

ls_parents_data = list(df['ls_parents'])


G = nx.DiGraph()
for parents in ls_parents_data:
    for i in range(len(parents) - 1):
        parent = parents[i]
        child = parents[i + 1]
        G.add_edge(child,parent)

def compute_depth(G, node, depth=0):
    """calculate depth of each node"""
    node_depth[node] = depth
    for child in G.neighbors(node):
        compute_depth(G, child, depth + 1)

node_depth = {}
# get root
roots = [node for node in G.nodes() if G.in_degree(node) == 0]

# calculate depth for roots and sort
for root in roots:
    compute_depth(G, root)
sorted_nodes = sorted(G.nodes())

# set position
pos = {}
for i, node in enumerate(sorted_nodes):
    x = i
    y = -node_depth[node] * 2  
    pos[node] = (x, y)

# set color
node_colors = []
for node in G.nodes():
    is_root = False
    # check whether the node is 0 node (root)
    for parents in ls_parents_data:
        if node == parents[0]:
            is_root = True
            break
    if is_root:
        node_colors.append('red')  #root
    else:
        node_colors.append('lightblue')  # interior node

#make a plot
plt.figure(figsize=(10, 8))
nx.draw(G, pos, with_labels=True, node_size=300, node_color=node_colors, font_size=10, font_weight='bold', edge_color='gray')
plt.title("Tree Structure (Left to Right by Node Value, with Depth)")


#change plot into newick format
def to_newick(G, root):
    def recurse(node):
        # get descendant
        children = list(G.neighbors(node))
        if not children:  # if no, return itself
            return str(node)  
        else:
            children_str = ",".join([recurse(child) for child in children])
            return f"({children_str}){str(node)}"  
    return recurse(root) + ";"

# get root
root = roots[0]

newick_tree = to_newick(G, root)


folder_path = "/Users/alexgjl/Desktop/final_data"   
file_path = os.path.join(folder_path, "tree_output.nwk")



with open(file_path, "w") as file:
    file.write(newick_tree)











