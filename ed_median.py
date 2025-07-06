import pandas as pd



#os.chdir("/Volumes/Jialiang/ed_arrangement")##change this to the path that the files existed in your computer

ed = pd.read_csv('ed.csv')

"""
ed2 = pd.read_csv('ed2.csv')
ed3 = pd.read_csv('ed3.csv')
ed4 = pd.read_csv('ed4.csv')
ed5 = pd.read_csv('ed5.csv')
ed6 = pd.read_csv('ed6.csv')
ed7 = pd.read_csv('ed7.csv')
ed8 = pd.read_csv('ed8.csv')
"""

edvals = ed.drop(columns=['Unnamed: 0', 'id', 'parent', 'ott', 'real_parent'])

#select 1000 columns

edvals = edvals.iloc[:, :1000]
edvals.to_csv("ed_values.csv",encoding = "gbk")
edvals['median'] = edvals.median(axis=1)


ed_med = pd.concat([ed[['Unnamed: 0', 'id', 'parent', 'ott', 'real_parent']], edvals['median']], axis=1)


ed_med.to_csv("ed_median.csv",encoding = "gbk")
