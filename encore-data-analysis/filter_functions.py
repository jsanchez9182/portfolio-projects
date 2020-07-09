# -*- coding: utf-8 -*-
"""
Created on Sun Jul  5 10:52:28 2020

@author: joewu
"""
import seaborn as sns
import matplotlib.pyplot as plt

def filter_data(df,col,val):
    """ filter a dataframe based on a value
    in a specified column"""
    data_subset = df.loc[df[col] == val]
    return data_subset
    
def filtered_grouped_countplot(df,col,val,subcol,
                               figsize=(10,10),legend_size=12,
                               xrange=(-0.5,0.5)):
    """ filter a dataframe based on value in column
    and then make a grouped countplot where subcol
    specifies the groups used"""
    data_subset = filter_data(df,col,val)
    plt.figure(figsize=figsize)
    ax = sns.countplot(data_subset[col],
                       hue = data_subset[subcol])
    ax.set_xticklabels([f'Issue: {val}'])
    plt.xlabel('')
    plt.ylabel('Number of Complaints')
    plt.legend(prop={'size':legend_size})
    plt.xlim(xrange)
    plt.show() 