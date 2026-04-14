# -*- coding: utf-8 -*-
"""
Created on Wed Sep  7 09:57:52 2022

@author: Savannah Swinea
         Northeastern University
"""
# Step 0: import necessary packages (with nicknames)

import matplotlib.pyplot as plt; plt.rcdefaults()
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import networkx as nx

# Step 1: Decide your aggregation technique and name your output files

# Line 174: edit this option based on if you want mean or median, and zeros included or excluded

# Lines 182, 193, 198, 199: name files based on your dataset

# In[1]

# Step 2: Reading in data using pandas and configuring node and edge sheets

# First make sure your Python script (this document) and your edge list are in the same folder.

# Then make sure your working directory is set correctly.  In the top-right corner you'll see a path that indicates your working directory.
# Set that equal to where your files are.  

excel_file = "./EdgeNodeList.xlsx" # Edit this to the name of your edge list Excel file
# This is a relative path within your current working directory.

df_node = pd.read_excel(excel_file, header = None, sheet_name = 1, engine = 'openpyxl') # This reads the node sheet in your file, which is the second listed sheet,
# hence sheet_name = 1.  This is read in as a dataframe object with Pandas.
df_node.head() # Did the data read in correctly? Run this to find out.

df_edge = pd.read_excel(excel_file, header = None, sheet_name = 0, engine = 'openpyxl') # Same idea here, but this reads in your edge sheet.
df_edge.head() # Again, check that the data look right.

#______________________________________________________________________________

# Step 3: Get your adjacency matrices for each respondent set up 

nnod = df_node.shape[0] - 1 # This uses the number of rows in the node sheet (minus the header) to find out the number of nodes in your models.
nedg = df_edge.shape[0] - 1 # This uses the number of rows in the edge sheet (minus the header) to find out the number of edges in your models.
nindiv = df_edge.shape[1] - 3 # This uses the number of columns in the edge sheet (minus the first three columns) to find out how many individual
# models you have.

Allparticipants={} # Creating an empty dictionary for participants
IDs = []  # Creating an empty list so that each participant has a unique ID/number

# Why use a dictionary or a list?  Dictionaries hold key-value pairs that are unique.  

Nodes_ID = {} # Creating an empty dictionary for nodes
for nod in range(nnod): # For every number in the range of numbers among the number of nodes you have 
    Nodes_ID[nod] = df_node.iat[nod+1,1] # This assigns each unique node name (value) to its corresponding number (key)
# Try typing Nodes_ID into your console to see the number-node name pairs

All_Nodes = list(Nodes_ID.values()) # Creating a list of your node names
N_nodes = len(All_Nodes) # Outputs the number of nodes you have

for indiv in range (nindiv): # For every number in the range of numbers among the number of individuals you have
    matrix = np.zeros((nnod,nnod)) # Outputs a matrix with dimensions equaling your number of nodes that is full of zeros
    IDs.append(df_edge.iat[0,3+indiv]) # Grabs the unique individual identifier (R_abc123 or something similar) and appends that to the IDs list
    
    for edg in range (1,nedg+1): # From 1 to the # of edges + 1 (basically helps you skip the header): iterating along the first matrix axis
        for nod in range (1,nnod+1): # From 1 to the # of nodes + 1 (basically helps you skip the header)
            if df_edge.iat[edg,0] == df_node.iat[nod,0]: # If the value in the first column (node number) matches in the node and edge sheets
                source_in = nod-1 # Your new variable source_in is equal to the node number 
                for nod2 in range (1,nnod+1): # Now you're iterating again, but along the next axis of the matrix
                    if df_edge.iat[edg,1] == df_node.iat[nod2,0]: # If 
                        target_in = nod2-1
                        #print (target_in)
                        W = df_edge.iat[edg,3+indiv]
                        matrix[source_in,target_in]= W # places edge value where it belongs in the adjacency matrix
    
    Allparticipants[df_edge.iat[0,3+indiv]]= matrix # Allparticipants is where respondent ID and corresponding matrix are stored
    
# To call a certain model: Allparticipants['ID']


# In[2]:

def FCM(ID):
    '''Generate an FCM'''
    
    adj = Allparticipants[ID]
    FCM = nx.DiGraph(adj)
         
    return FCM 

# nx.draw(FCM('FS_3qaHdEeUk1ubBgt')) will make a simple plot of a specified model

# In[3]:

# https://www.udacity.com/blog/2021/11/__init__-in-python-an-overview.html#:~:text=The%20__init__%20method%20is%20the%20Python%20equivalent%20of,is%20only%20used%20within%20classes.

class Agents (object): # Agents is a class
    
    def __init__ (self,ID): # defines the __init__ method which initializes an ID attribute  
        self.ID = ID # self is a keyword that binds attributes of the object to the arguments received
        self.FCM = FCM(self.ID) # calling the FCM function

# In[4]:
    
n_concepts = N_nodes
def aggregation(agent_list,How):
    
    if How == "Mean_In":  # Take the Mean Including Zeroes
        adj_ag=np.zeros((n_concepts,n_concepts))
        All_ADJs = []
        for agent in agent_list:
            All_ADJs.append(nx.to_numpy_matrix(agent.FCM))
        from statistics import mean
        for i in range (n_concepts):
            for j in range (n_concepts):
                a = [ind[i,j] for ind in All_ADJs]
                adj_ag[i,j] = mean(a)
        aggregated_FCM = adj_ag   
        
#--------------------------------------------------------------------        
    if How == "Mean_Ex": #Take the Mean Excluding Zeroes
        adj_ag=np.zeros((n_concepts,n_concepts))
        All_ADJs = []
        for agent in agent_list:
            All_ADJs.append(nx.to_numpy_matrix(agent.FCM))
        from statistics import mean
        for i in range (n_concepts):
            for j in range (n_concepts):
                a = [ind[i,j] for ind in All_ADJs if ind[i,j]!=0]
                if len(a)!=0:
                    adj_ag[i,j] = mean(a)
        aggregated_FCM = adj_ag   
        
#----------------------------------------------------------------------
    if How == "MED_Ex": #Take the Median Excluding Zeroes
        adj_ag=np.zeros((n_concepts,n_concepts))
        All_ADJs = []
        for agent in agent_list:
            All_ADJs.append(nx.to_numpy_matrix(agent.FCM))
        from statistics import median as med
        for i in range (n_concepts):
            for j in range (n_concepts):
                a = [ind[i,j] for ind in All_ADJs if ind[i,j]!=0]
                if len(a)!=0:
                    adj_ag[i,j] = med(a)
        aggregated_FCM = adj_ag   
        
#----------------------------------------------------------------------
    if How == "MED_In": #Take the Median Inluding Zeroes
        adj_ag=np.zeros((n_concepts,n_concepts))
        All_ADJs = []
        for agent in agent_list:
            All_ADJs.append(nx.to_numpy_matrix(agent.FCM))
        from statistics import median as med
        for i in range (n_concepts):
            for j in range (n_concepts):
                a = [ind[i,j] for ind in All_ADJs]
                if len(a)!=0:
                    adj_ag[i,j] = med(a)
        aggregated_FCM = adj_ag   
        
#----------------------------------------------------------------------    
    return aggregated_FCM


# In[5]:
# Generating agents

All_agents=[] # initializing a list

for i in IDs:
    a = Agents(ID=i)
    All_agents.append(a)
  

# In[6]:
# Aggregation of individual FCMs. Choose "Mean_Ex" "Mean_In" "MED_Ex" "MED_In"

Aggregated_group_map = aggregation(All_agents,"Mean_Ex") # edit if necessary


# In[7]
Aggregated_map = nx.DiGraph(Aggregated_group_map)
df_nod = pd.DataFrame.from_dict(Nodes_ID, orient='index', dtype=None, columns=['Label'])
# df_nod.to_csv('./All_Nodes_20220926_meanex.csv', index=True, header=True) # edit if necessary

ed_dic = {}; 
ed_dic['Source'] =[]; ed_dic['Target'] = []; ed_dic['Type'] = []; ed_dic['Weight'] =[]
for e in Aggregated_map.edges():
    ed_dic['Source'].append(e[0])
    ed_dic['Target'].append(e[1])
    ed_dic['Type'].append('Directed')
    ed_dic['Weight'].append(Aggregated_map[e[0]][e[1]]['weight'])
    
df= pd.DataFrame(ed_dic)
# df.to_csv('./All_Edges_20220926_medex_CK.csv', index=False,header=True) # edit if necessary

# In[8]
AJMX= nx.to_numpy_matrix(Aggregated_map, weight='weight', nonedge=0.0)
df_AJMX = pd.DataFrame(AJMX, index=df_nod['Label'], columns=df_nod['Label']) 
df_AJMX.to_csv('./Adjacency_Matrix_nXn.csv', index=True, header=True) # edit if necessary
df_AJMX.to_excel('./Adjacency_Matrix_nXn.xlsx', index=True, header=True) # edit if necessary