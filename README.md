# Phylogenetic-Diversity-across-the-complete-tree-of-life

#read_json_get_date_dataframe 
The json field comes from the Open Tree of Life in which each one or two OTT_IDs have a date estimate. The #read_json_get_date_dataframe field will help us to find corresponding node ID for those date estimates and generate a table named "latest_node_dates”.

#bootstrap_analysis
With the "latest_node_dates” table generated, #bootstrap_analysis can use this table to randomly select 50% of the node date estimates, use the selected date estimates to calculate ED scores and repeat this step for 100 times. That will output a table named “ed_bootstrap(100)”.

#pd_estimate_for_ordered_nodes 
This script will help us to calculate total PD for all the interior nodes across the tree of life based on the calculated ED scores. 

#figures_script
This is a R script that can generate all the figures in this paper.
