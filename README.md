# Phylogenetic-Diversity-across-the-complete-tree-of-life

#read_json_get_date_dataframe 
The json field comes from the Open Tree of Life in which each one or two OTT_IDs have a date estimate. The #read_json_get_date_dataframe field will help us to find corresponding node ID for those date estimates and generate a table named "latest_node_dates”.

#bootstrap_analysis
With the "latest_node_dates” table generated, #bootstrap_analysis can use this table to randomly select 50% of the node date estimates, use the selected date estimates to calculate ED scores and repeat this step for 100 times. That will output a table named "ed_bootstrap(100)". We also have scripts for submitting the bootstrap analysis to HPC cluster. These scripts include: ED_score_for_hpc.py, which can calculate ED score for the leaves table and return a list of ED score; driver.py, which can run the ED_score_for_hpc and collect its output; ED_data_collection1-10.sh, which can change the random seed for the ED_score_for_hpc.py and split the 100 times bootstrap process into 10 seperate process; and the merge_part.sh, which can merge the output of ED_data_collection1-10.sh into a dataframe of ED score. After the table of ED scores are generated through merge_part.sh, run the transpose_ed_table.py to generate a table of ED scores.

#pd_estimate_for_ordered_nodes 
This script will help us to calculate total PD for all the interior nodes across the tree of life based on the calculated ED scores. 

#phyloinfo_for_edge20_species
This script calculates cumulative ED value for each interior node of the top 20 EDGE species

#figures_script
This is a R script that can generate all the figures in this paper. Estimation of EDGE scores for those IUCN evaluated species is also given in this part.
