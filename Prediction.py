# -*- coding: utf-8 -*-
"""
Created on 2023.09.18

@author: Ke Zhang
"""


## 确认python的版本运行与终端运行的版本一致【python --version】
import sys; print(sys.version)


## import the needed library 

import pandas as pd
from pandas import ExcelWriter
from pandas import Series, DataFrame

import numpy as np
import os


import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns


from sklearn import preprocessing
from sklearn.preprocessing import label_binarize
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier

from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold

from sklearn import metrics 
from sklearn.metrics import roc_auc_score, roc_curve, accuracy_score, confusion_matrix
from sklearn.metrics import r2_score, precision_recall_curve, explained_variance_score
from sklearn.metrics import mean_squared_error

import lightgbm as lgb
from lightgbm import LGBMClassifier 

import shap
import scipy.stats as stats 
from scipy.stats.stats import spearmanr, pearsonr

shap.initjs()






# Pre-define 
# -----------------------------------------------------------------------------

'''
calculate MAPE, Mean Absolute Percentage Error（平均绝对百分比误差）
'''

def calculate_mape(y_true, y_pred):
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100



'''
kfold split samples based on individuals, 按照个人进行样本随机拆分
'''

def id_kfold(data, id, folds, seed): 
    idlist = list(set(id))
    np.random.seed(seed) 
    shuffled_idlsit = np.random.permutation(idlist) # 随机打乱向量  

    kfold = pd.DataFrame(np.array_split(shuffled_idlsit, folds))
    kfold_t = kfold.transpose()    

    kfold_melt = kfold_t.melt()
    kfold_melt = kfold_melt.dropna()
    kfold_melt.value = kfold_melt.value.astype(object)
    kfold_melt.columns = ['kfold', 'id']
    kfold_melt.dtypes

    input_data = pd.merge(kfold_melt, data, on='id', how='left')

    return input_data 




'''
calculate overall mean shap values for features importance ranking
'''

def calculate_meanSHAP(shap_fold, seed):  

    shap_abs = abs(shap_fold)
    shap_mean = pd.DataFrame(shap_abs.mean())
    shap_mean.columns = [seed]

    return shap_mean 




'''
merge predicted values and corresponding true values
'''

def merge_pred_true(folds, tests, preds, seed): 
    output_test = pd.DataFrame()
    for i in np.arange(0,folds):
        tests[i] = pd.DataFrame(tests[i])
        output_test = pd.concat([output_test, tests[i]])
    
    nrow = output_test.iloc[:,0].size
    output_test.index = np.arange(0,nrow)
    output_test.columns = ['true']


    output_pred = pd.DataFrame()
    for i in np.arange(0,folds):
        preds[i] = pd.DataFrame(preds[i])
        output_pred = pd.concat([output_pred, preds[i]], axis=0)
    
    nrow = output_pred.iloc[:,0].size
    output_pred.index = np.arange(0,nrow)  
    output_pred.columns = ['pred']
    
    output_total = []
    output_total = pd.concat([output_pred, output_test], axis=1)
    output_total.columns = [('pred_' + str(seed) ),('test_' + str(seed))]
 
    return output_total, output_test, output_pred




'''
evaluate performance of prediction models
'''

def eval_perf(y_true, y_pred, seed): 
    model_eval = pd.DataFrame()

    model_eval.loc[seed, 'Coefficient_of_determination'] = r2_score(y_true=y_true, y_pred=y_pred) 
    model_eval.loc[seed, 'explained_variance_score'] = explained_variance_score(y_true=y_true, y_pred=y_pred)

    model_eval.loc[seed, 'pearson_r'], model_eval.loc[seed, 'pearson_p'] = pearsonr(y_true, y_pred)
    model_eval.loc[seed, 'spearman_r'], model_eval.loc[seed, 'spearman_p'] = spearmanr(y_true, y_pred)
    model_eval.loc[seed, 'RMSE'] = mean_squared_error(y_true=y_true, y_pred=y_pred)
    model_eval.loc[seed, 'MAPE'] = calculate_mape(y_true=y_true, y_pred=y_pred)
     
    return model_eval













    


# Parameters settngs
# -----------------------------------------------------------------------------

root = 'root of path' 
os.chdir(root)

random_seedlist = pd.read_excel('seedlist.xlsx') 
seedlist = random_seedlist.iloc[0:1000,1]
date = '000000'


# set hyperparameters

lgb_params = { 
              'metric': 'l2',
              'learning_rate': 0.005,
              'feature_fraction': 0.2,
              'min_data_in_leaf': 15, 
              'early_stopping_rounds': None, 'n_estimators': 2000,
              'bagging_fraction': 0.8, 'bagging_freq': 1,
              'num_threads': 1, 'verbose': -1, 'silent': True}


folds = 10
n_bs = 100















# sort filelist of input features and output path 
# -----------------------------------------------------------------------------

"""

create filelist for loop, and make folders to save results

"""

feat_list = ['clinic','diet','metab_feces','metab_serum','microbiota','genetics']
otc_list = ['PGI']
filelist = pd.DataFrame(columns=['input_x', 'input_y', 'output_path'])

for feature in feat_list:
    
    for otc in otc_list:
         
        group = feature
        input_x = 'input/feat_' + feature + '.csv'
        input_y1 = 'input/otc_conti_' + otc + '.csv'
        output1 = os.path.join('output/') 
        
        # if not os.path.exists(output1): 
        #    os.makedirs(output1) 
              
        # 将路径添加到filelist中
        filelist = filelist._append({'group': group,'input_x': input_x, 'input_y': input_y1, 'output_path': output1}, ignore_index=True) 
    
filelist['output_path'] = filelist['output_path'].str.replace('\\', '/')













# build up the prediction model ----------------------------------------#

for index, row in filelist.iterrows():
    os.chdir(root) #修改工作路径

    tar = row['group']
    input_x = row['input_x']
    input_y = row['input_y']
    output_path = row['output_path']

    x_train=pd.read_csv(input_x) 
    y_train=pd.read_csv(input_y)
 
    meanSHAP_all = pd.DataFrame()  
    output_prediction_all = pd.DataFrame()
    model_eval_all = pd.DataFrame()
     
    for s in range(n_bs):   
     
        # modeling ----------------------------------------------------# 
        model=lgb.LGBMRegressor(**lgb_params) 
        kf = KFold(n_splits=folds, shuffle=True, random_state=seedlist[s])
        
        trn = x_train
        val = y_train
        
        tests = []
        preds = []  
        shap_fold = pd.DataFrame()
        
        
        i = 0
        for train_index, test_index in kf.split(trn):
            
            x1, x2 = trn.iloc[train_index], trn.iloc[test_index]
            y1, y2 = val.iloc[train_index], val.iloc[test_index]
        
            model.fit(x1, y1) 
            pred = model.predict(x2)
            
            tests.append(y2)
            preds.append(pred) 
            
            # SHAP Model interpreting ------------------------#
            
            explainer = shap.TreeExplainer(model)
            shap_values =  pd.DataFrame(explainer.shap_values(x2))
            # shap_values_2d = pd.DataFrame(shap_values[0])  
            shap_values.columns = trn.columns 
            shap_fold = pd.concat([shap_fold, shap_values], axis=0) # 按行合并 
            
            i+=1 
                 
        

        # calculate overall mean SHAP values-------------------------
        shap_mean = calculate_meanSHAP(shap_fold=shap_fold, seed=seedlist[s])
        meanSHAP_all = pd.concat([meanSHAP_all, shap_mean], axis=1)

        # output true and predicted value -------------------------------------
        output_total, output_test, output_pred = merge_pred_true(folds=folds, tests=tests, preds=preds, seed=seedlist[s])
        output_prediction_all = pd.concat([output_prediction_all, output_total], axis=1)

        # evaluate performance and output---------------------------------
        model_eval = eval_perf(y_pred=output_pred.pred, y_true=output_test.true, seed=seedlist[s])
        model_eval_all = pd.concat([model_eval_all, model_eval], axis=0)


    ## save results-------------------------------------------
    os.chdir('/root of path') #修改工作路径

    filename = ('lightGBM_results_' + tar + date + '.xlsx')
    writer = pd.ExcelWriter(filename)

    model_eval_all.to_excel(writer, sheet_name='performance')
    meanSHAP_all.to_excel(writer, sheet_name='meanSHAP')  
    output_prediction_all.to_excel(writer, sheet_name='prediction') 

    writer._save()












