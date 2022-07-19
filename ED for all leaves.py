import pandas as pd
import numpy as np

#fields
#use new node age
nodes = pd.read_csv("latest nodeages 0708.csv",low_memory=False)
nodes = pd.DataFrame(nodes, columns = ["Unnamed: 0","id","parent","leaf_lft","leaf_rgt","unnamed:0","average"])

leaves1 = pd.DataFrame(df_leaves,columns = ["id","parent","ott"])




def find_parents(a):   ##find parents based on a leaf id(a)
    list_parentsl = []
    cp = leaves1.iat[int(a)-1,2]
    while cp != -27400288:
        list_parentsl.append(cp)
        cp = nodes.iat[int(cp)-1,2]
    return list_parentsl


def find_des(a): ##return 1/descendants for a node id in the whole table
    return(1/(nodes.iloc[a-1,4]-nodes.iloc[a-1,3]+1))

def find_age(a):
    return(nodes.iloc[a-1,6])

def ed(list_try):###try must be sorted from small to big！！！！！！！！！because this function is based on index
    numround = -1 #always equal to the index of i
    count = 0#number of nodes with age = 0 between two ages != 0
    list1 = [] #store the 1/num of descendants for age = 0,update in each loop
    listed = [] #calculation of ed
    list_end = []#to see whether this node is the last one with age
    for i in list_try:
        if list_try.index(i) == 0:##skip the first node
            numround += 1
        else:
            if find_age(i) == 0:
                numround += 1
                for j in range(list_try.index(i),len(list_try)):##j is an index
                    list_end.append(find_age(list_try[j]))##add all the nodes behind list[j] include itself, *****list end is a list of age, only length make sense
                ###print(list_end)######
                        #a list with all nodes behind
                if sum(list_end) == 0:####all the ages of nodes behind is zero
                    if list_try.index(i) != len(list_try)-1:#not the last one                    
                        for k in range(list_try.index(i),len(list_try)):###k is an index
                            #print(list_try[k])######
                            list1.append(find_des(list_try[k]))######age:(find_age(list_try[list_try.index(i)-1])
                        #print(list1)
                        ed = ((find_age(list_try[(list_try.index(i)-1)]))/(len(list_end)+1)) * (1+sum(list1))
                        #print(ed)#######
                        #print(find_age(list_try[(list_try.index(i)-1)]))########
                            #* (sum(list1.pop(-1))+find_des(list_try[list_try.index(i)-1]))+ (find_ages(list_try[list_try.index(i)-1])/(len(list_end)+1))##
                            #print(ed)
                        listed.append(ed)
                        break
                    else:
                        ed = ((find_age(list_try[(list_try.index(i)-1)]))/2)*(1+find_des(i))
                            #print(ed)
                            #listed.append(ed)
                        listed.append(ed)
                        break
                else:
                    list_end = []
                    list1.append(find_des(i))
                    count += 1
            if find_age(i) != 0:
                numround += 1
                if list_try.index(i) != len(list_try)-1:##not the last one
                        #print(count)##count
                    branch_length = ((find_age(list_try[(numround-count-1)])) - (find_age(i))) / (1+count) #average branch length between two nodes with ages                         
                    ed = branch_length*(sum(list1)+(find_des(i)))
                    #print(ed)
                    listed.append(ed)
                    count = 0#update count after calculate the ed
                    list1 = []#update the list1
                else:
                    branch_length = ((find_age(list_try[(numround-count-1)])) - (find_age(i))) / (1+count)
                    ed = find_age(i) + branch_length*(sum(list1)+(find_des(i)))
                    listed.append(ed)
                    break
    #listed = sorted(listed,reverse = True)
    return(sum(listed))


##calculate the ed contributed by the terminal branch
def ed_terminal(list_try):###list try must be sorted from big to small
    list_try = sorted(list_try,reverse = True)
    count = 1
    for i in list_try:
        if find_age(i) == 0:
            count += 1
        else:
            ed_terminal = find_age(i)/count
            return(ed_terminal)
    

##calculate ed

listed = []
ed_by_terminal_branch = []
for row in leaves1.itertuples():
    leaf_id = getattr(row,"id")
    allparents = find_parents(leaf_id)
    allparents = sorted(allparents)
    temp = ed(allparents)
    temp1 = ed_terminal(allparents)
    listed.append(temp)
    ed_by_terminal_branch.append(temp1)
    
leaves1["ed"] = listed
leaves1["ed by terminal branch"] = ed_by_terminal_branch
leaves1.to_csv("leaves with ed.csv",encoding = "gbk")

