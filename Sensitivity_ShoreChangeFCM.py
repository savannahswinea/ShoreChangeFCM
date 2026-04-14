# -*- coding: utf-8 -*-
"""
Created on Mon Feb 9 12:35:33 2023

@author: Savannah Swinea
         Northeastern University
"""

# 0. Make sure your working directory (top right corner) is pointing to where this file and the initialization file are located

# THINGS YOU CAN EDIT IN THE CODE:
    
    # Noise_Threshold
    # resid (in infer_steady and infer_scenario)
    # Principles
    # list_of_concepts_to_run
    # function_type
    # infer_rule
    # change_level
    

import matplotlib.pyplot as plt; plt.rcdefaults()
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import math
import networkx as nx

#%%

# 1. Convert each entry in adjacency list to a matrix

excel_file = "./EdgeNodeList.xlsx" # file with all FCMs written in edge list format

df_node = pd.read_excel(excel_file, header = None, sheet_name = 1) # df containing nodes (from second sheet of Excel file) 
df_edge = pd.read_excel(excel_file, header = None, sheet_name = 0) # df containing edges for all FCMS (from first sheet of Excel file)

nnod = df_node.shape[0] - 1 # new variable defined as number of nodes
nedg = df_edge.shape[0] - 1 # new variable defined as number of edges
nindiv = df_edge.shape[1] - 3 # new variable defined as number of individual FCMs

Allparticipants={} # Creating an empty dictionary for participants
IDs = []  # Creating an empty list so that each participant has a unique ID/number  

Nodes_ID = {} # Creating an empty dictionary for nodes
for nod in range(nnod): # For every number in the range of numbers among the number of nodes you have 
    Nodes_ID[nod] = df_node.iat[nod+1,1] # This assigns each unique node name (value) to its corresponding number (key)
# Try typing Nodes_ID into your console to see the number-node name pairs

All_Nodes = list(Nodes_ID.values()) # Creating a list of your node names
N_nodes = len(All_Nodes) # Outputs the number of nodes you have

for indiv in range (nindiv): # For every individual in the range of numbers among the number of individuals you have
    matrix = np.zeros((nnod,nnod)) # Outputs a matrix with dimensions equaling your number of nodes that is full of zeros
    IDs.append(df_edge.iat[0,3+indiv]) # Grabs the unique individual identifier (R_abc123 or something similar) and appends that to the IDs list
    
    for edg in range (1,nedg+1): # From 1 to the # of edges + 1 (basically helps you skip the header): iterating along the first matrix axis
        for nod in range (1,nnod+1): # From 1 to the # of nodes + 1 (basically helps you skip the header)
            if df_edge.iat[edg,0] == df_node.iat[nod,0]: # If the value in the first column (node number) matches in the node and edge sheets
                source_in = nod-1 # Your new variable source_in is equal to the node number 
                for nod2 in range (1,nnod+1): # Now you're iterating again, but along the next axis of the matrix
                    if df_edge.iat[edg,1] == df_node.iat[nod2,0]:
                        target_in = nod2-1
                        W = df_edge.iat[edg,3+indiv]
                        matrix[source_in,target_in]= W # places edge value where it belongs in the adjacency matrix
    
    Allparticipants[df_edge.iat[0,3+indiv]]= matrix # Allparticipants dictionary is where respondent ID and corresponding matrix are stored
    
# To call a certain model: Allparticipants['ID']

#%%

# This function defines the different squashing functions.  You will call this in the next cell block

def TransformFunc (x, n, f_type):
    
    if f_type == "sig": # sigmoid
        x_new = np.zeros(n)
        for i in range (n):
            x_new[i]= 1/(1+math.exp(-1*x[i])) # edit Lambda here (0-10)
            # x_new[i]= 1/(1+math.exp(-Lambda*x[i]))
        return x_new    

    
    if f_type == "tanh": # hyperbolic tangent
        x_new = np.zeros(n)
        for i in range (n):
            x_new[i]= math.tanh(1*x[i]) # edit Lambda here (0-10)
            # x_new[i]= math.tanh(Lambda*x[i])
        return x_new
    
    if f_type == "biv": # bivalent
        x_new = np.zeros(n)
        for i in range (n):
            if x[i]> 0:
                x_new[i]= 1
            else:
                x_new[i]= 0
        
        return x_new
    
    if f_type == "triv": # trivalent
        x_new = np.zeros(n)
        for i in range (n):
            if x[i]> 0:
                x_new[i] = 1
            elif x[i]==0:
                x_new[i] = 0
            else:
                x_new[i] = -1
        
        return x_new

#%%

# 2. Iterate and run the scenario on each matrix

# Initializing some stuff outside of the for loop

list_concepts = list(Nodes_ID.values())
list_concepts.insert(0, 'Label')
overall_DF = pd.DataFrame(list_concepts) # this is where scenario results will be appended
a = 0

# For loop begins here

for key in Allparticipants: # for each respondent in the dictionary which contains all the respondent matrices
    
    a += 1 # index used at the end
    
    FCM = pd.DataFrame(Allparticipants[key]) # converts FCM data stored in dictionary to dataframe
    
    # getting the data frame set up to have the right attributes
    
    nodeValuesList = list(Nodes_ID.values()) # making a list of nodes
    FCM.loc[-1] = nodeValuesList # makes the list of nodes the 0th column in the FCM dataframe
    FCM.index = FCM.index + 1
    FCM = FCM.sort_index()
    nodeValuesList2 = nodeValuesList # making c copy of the nodes list
    nodeValuesList2.insert(0, 'Label') # adding Label as the 0th item in this copied nodes list
    FCM.insert(0, 'newcol', nodeValuesList2) # inserting this fixed list of nodes (with the Label title) into the dataframe

    # Initializing some stuff within the for loop

    Adj_matrix = np.zeros((nnod,nnod))
    activation_vec = np.ones(nnod)
    node_name = {}
    n_concepts = nnod
    
    Noise_Threshold = float(0) # YOU DECIDE (between 0-1)

    for i in range (1,n_concepts): # this for loop removes edges which are smaller in magnitude than the noise threshold
        for j in range (1,n_concepts):
            if abs(FCM.iat[i,j])<=Noise_Threshold:
                Adj_matrix[i-1,j-1]=0
            else:
                Adj_matrix[i-1,j-1]=FCM.iat[i,j]
            
            
    Concepts_matrix = []
    for i in range (1,nnod+1):
        Concepts_matrix.append(FCM.iat[0,i])

    G = nx.DiGraph(Adj_matrix) # making a NetworkX graph out of the initialized adjacency matrix
    for nod in G.nodes():
        node_name[nod] = FCM.iat[nod+1,0] 


#______________________________________________________________________________

    def infer_steady (init_vec = activation_vec , AdjmT = Adj_matrix.T \
                      , n = n_concepts , f_type ="sig", infer_rule = "mk"):
        
        act_vec_old = init_vec
    
        resid = 1
        while resid > 0.00001:  # YOU DECIDE (here you have to define the stopping rule for steady state calculation)
            act_vec_new = np.zeros(n)
            x = np.zeros(n)
                

            if infer_rule == "k":
                x = np.matmul(AdjmT, act_vec_old)
            if infer_rule == "mk":
                x = act_vec_old + np.matmul(AdjmT, act_vec_old)
            if infer_rule == "r":
                x = (2*act_vec_old-np.ones(n)) + np.matmul(AdjmT, (2*act_vec_old-np.ones(n)))
            # print(act_vec_old)  
            act_vec_new = TransformFunc (x ,n, f_type)
        
            resid = max(abs(act_vec_new - act_vec_old))

        
            act_vec_old = act_vec_new
        return act_vec_new
#______________________________________________________________________________
    def infer_scenario (Scenario_concepts,change_level, init_vec = activation_vec , AdjmT = Adj_matrix.T \
                        , n =n_concepts , f_type="sig", infer_rule ="mk" ):
        act_vec_old = init_vec
    
        resid = 1
        while resid > 0.00001:  # YOU DECIDE (here you have to define the stopping rule for steady state calculation)
            act_vec_new = np.zeros(n)
            x = np.zeros(n)
        
            if infer_rule == "k":
                x = np.matmul(AdjmT, act_vec_old)
            if infer_rule == "mk":
                x = act_vec_old + np.matmul(AdjmT, act_vec_old)
            if infer_rule == "r":
                x = (2*act_vec_old-np.ones(n)) + np.matmul(AdjmT, (2*act_vec_old-np.ones(n)))
            
            act_vec_new = TransformFunc (x ,n, f_type)
        
            for c in  Scenario_concepts:
            
                act_vec_new[c] = change_level[c]
        
            
            resid = max(abs(act_vec_new - act_vec_old))
        
            act_vec_old = act_vec_new
        return act_vec_new
#______________________________________________________________________________


    Principles = ["Marine Life", "Storm Protection", "Water Quality"] # YOU DECIDE (list the concepts you want to be principles (basically if you only wanted to output results for certain concepts in a graph)


    prin_concepts_index = []
    for nod in node_name.keys():
        if node_name[nod] in Principles:
            prin_concepts_index.append(nod)


    list_of_concepts_to_run = ['Seawalls and Hardened Shorelines', 'Salt Marshes']  # YOU DECIDE (list the concept(s) you want to activate)    

#______________________________________________________________________________
    function_type = "sig" # YOU DECIDE (squashing function. options: biv, triv, sig, tanh)
    infer_rule = "k" # YOU DECIDE (inference rules. options: k, mk, r)
    change_level = {'Seawalls and Hardened Shorelines': float(-0.05), 'Salt Marshes': float(0.05)} # YOU DECIDE (activation level for each concept to activate, between -1 and 1)

    change_level_by_index = {} 
    for name in change_level.keys():
        change_level_by_index[Concepts_matrix.index(name)] = change_level[name]

    Scenario_concepts = [] 
    for name in list_of_concepts_to_run:
        Sce_Con_name =name
        Scenario_concepts.append(Concepts_matrix.index(Sce_Con_name))

    
    change_IN_PRINCIPLES = []
    

    SteadyState = infer_steady (f_type=function_type, infer_rule = infer_rule) 
    ScenarioState = infer_scenario (Scenario_concepts,change_level_by_index ,f_type=function_type, infer_rule = infer_rule)
    change_IN_ALL = ScenarioState - SteadyState

    for c in Scenario_concepts:
        change_IN_ALL[c] = 0

    for i in range (len(prin_concepts_index)): 
        change_IN_PRINCIPLES.append(change_IN_ALL[prin_concepts_index[i]])

    changes_dic ={}
    for nod in G.nodes():
        changes_dic[node_name[nod]] = change_IN_ALL[nod]
            

    # maybe change how this works in the future: assign ALL scenario changes to one dictionary and then concatenate that dic to dataframe
    list_scenario_changes = list(changes_dic.values())
    list_scenario_changes2 = list_scenario_changes
    list_scenario_changes2.insert(0, key) 
    overall_DF[a] = list_scenario_changes2
    

# Writing scenario results from individual FCMs to CSV

overall_DF.to_csv('Sensitivity_SeawallMar.csv')
    
   

   
    