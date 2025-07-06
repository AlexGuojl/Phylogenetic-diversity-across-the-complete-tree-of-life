import pandas as pd
import os


pd = pd.read_csv('pd.csv',low_memory=False)


pdvals = pd.drop(columns=['Unnamed: 0', 'id', 'name', 'ott', 'richness'])


pdvals = pd.concat([pdval1,pdval2,pdval3,pdval4,pdval5,pdval6,pdval7,pdval8],axis = 1)
pdvals = pdvals.iloc[:, :1000]
pdvals['median'] = pdvals.median(axis=1)


pd_mpd = pd.concat([pd1[['Unnamed: 0', 'id', 'name', 'ott', 'richness']], pdvals['median']], axis=1)


pd_mpd.to_csv("pd_median.csv",encoding = "gbk")

