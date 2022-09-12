#!/usr/bin/env python
# coding: utf-8

# # Functions

# In[2]:


import lightgbm as lgb
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
import sklearn

from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

from sklearn.metrics import f1_score
from sklearn.metrics import confusion_matrix
from sklearn.metrics import precision_recall_curve
from sklearn.metrics import plot_precision_recall_curve
from sklearn.metrics import accuracy_score, confusion_matrix,roc_curve, roc_auc_score, precision_score, recall_score, precision_recall_curve
from sklearn.metrics import classification_report
from sklearn.metrics import plot_confusion_matrix

#Model selection
from sklearn.model_selection import train_test_split
from sklearn.model_selection import learning_curve, GridSearchCV
from sklearn.model_selection import validation_curve
from sklearn.model_selection import RepeatedKFold

#Feature selection
from sklearn.feature_selection import VarianceThreshold
from sklearn.feature_selection import SelectFromModel
from numpy import sort


#Models
from sklearn.dummy import DummyClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn import linear_model
from sklearn.ensemble import AdaBoostClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import mean_squared_error
#from imblearn.ensemble import BalancedRandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LogisticRegressionCV
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LassoLarsIC
from xgboost import plot_importance
from xgboost import XGBClassifier

#RDS
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri


# In[3]:


def save_result(Nombre,y_test, y_pred):
    report = classification_report(y_test, y_pred, output_dict=True)
    df = pd.DataFrame(report).transpose()
    df = df["f1-score"]
    F1_score = df[1]
    AUC = roc_auc_score(y_test,y_pred)
    tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()
    Specificity = (tn/(tn+fp))*100
    Sensitivity = (tp/(tp+fn))*100
    Precision = (tp/(tp+fp))*100
#     result = {
#     "Nombre" : Nombre,
#     "AUC": AUC,
#     "F1_score": F1_score,
#     "Specificity": Specificity,
#     "Sensitivity": Sensitivity
#     }
    result = pd.DataFrame([AUC,F1_score,Specificity,Sensitivity,Precision]) #Nombre
    result = result.rename(columns={0: Nombre})
    result = result.rename(index={0: "AUC", 1: "F1", 2: "Specificity", 3: "Sensitivity", 4: "Precision"})
    return result


# In[4]:


def save_result_1(Nombre,y_test, y_pred):
    report = classification_report(y_test, y_pred, output_dict=True)
    df = pd.DataFrame(report).transpose()
    df = df["f1-score"]
    F1_score = df[1]
    AUC = roc_auc_score(y_test,y_pred)
    tn, fp, fn, tp = confusion_matrix(y_test, y_pred).ravel()
    Specificity = tn/(tn+fp)
    Sensitivity = tp/(tp+fn)
    return AUC,F1_score,Sensitivity,Specificity


# In[5]:


def evaluation_fun(Y_test,y_pred):
    print(f'Accuracy Score: {accuracy_score(Y_test,y_pred)}')
    print(f'Area Under Curve: {roc_auc_score(Y_test, y_pred)}')
    print(f'Recall score: {recall_score(Y_test,y_pred)}')
    print("")
    matrix = confusion_matrix(Y_test, y_pred)
    tn, fp, fn, tp = confusion_matrix(Y_test, y_pred).ravel()

    print('Specificity:', tn/(tn+fp))
    print('Sensitivity:', tp/(tp+fn))
    print(f'F1 score: {f1_score(Y_test,y_pred)}')
    print(f'Precision score: {precision_score(Y_test,y_pred)}')
    print(matrix)
    print("")
    print(classification_report(Y_test, y_pred))
    plt.figure(figsize=(7,7))
    sns.heatmap(matrix,annot=True, square = True, fmt='g')
    plt.show()
    return


# In[6]:


def Best_f1_score(ypred_55,y_test):    
    f1_score_fin = 0
    threshold = 0
    ypred_55_fin = []
    maximo = max(ypred_55)
    print("Max value with the prediction " + str(maximo))
    explore = np.linspace(0,maximo,num = 50)
    for i in explore:
        ypred_55_now = ypred_55
        ypred_55_now = np.where(ypred_55 > i, 1, 0)
        f1_score_now = f1_score(y_test,ypred_55_now)
        if f1_score_now > f1_score_fin:
            f1_score_fin = f1_score_now
            threshold = i
            ypred_55_fin = ypred_55_now
    return f1_score_fin, threshold, ypred_55_fin


# In[22]:


def filter_data(iso2): #,number
    #df = pd.read_csv(iso2+'_'+str(number)+'.csv')
    
    readRDS = robjects.r['readRDS']
    
    #My jupyter docker
#     df_1 = readRDS("C:/Users/Administrator/Documents/Data_2020_dummies/2020-Q2/" + iso2 + ".rds")
    df_1 = readRDS("/home/coronasurveys/UMD/2020-Q2/dummies/" + iso2 + ".rds")
    df_2 = readRDS("/home/coronasurveys/UMD/2020-Q3/dummies/" + iso2 + ".rds")
    df_3 = readRDS("/home/coronasurveys/UMD/2020-Q4/dummies/" + iso2 + ".rds")
    df_1 = pandas2ri.rpy2py_dataframe(df_1)
    df_2 = pandas2ri.rpy2py_dataframe(df_2)
    df_3 = pandas2ri.rpy2py_dataframe(df_3)
    
#     print("2020 " + str(df_old.shape))
#     print("2021 " + str(df.shape))

    #Server 
    #df_1 = pyreadr.read_r("/home/coronasurveys/UMD/2021-Q1/dummies/" + iso2 + ".rds")
    #df_2 = pyreadr.read_r("/home/coronasurveys/UMD/2021-Q2/dummies/" + iso2 + ".rds")
    #df_3 = pyreadr.read_r("/home/coronasurveys/UMD/2021-Q3/dummies/" + iso2 + ".rds")
    #df_4 = pyreadr.read_r("/home/coronasurveys/UMD/2021-Q4/dummies/" + iso2 + ".rds")

    df_total = [df_1,df_2,df_3]
    
    
    df = pd.DataFrame(pd.concat(df_total,join='inner', ignore_index=True)) 
    print("before removing non tested " + str(df.shape))
    df = df[(df['B7.1']==1) & (df['B8.NA']==0) & (df['B8.3']==0)]
    print("after removing non tested " + str(df.shape))
    print("Positives cases " + str(len(df[(df['B8.1']==1)])))
    print("Asymptomatic cases " + str(len(df[(df['B8.1']==1) & ((df['B1_1.1']==0) & (df['B1_2.1']==0) & (df['B1_3.1']==0) & (df['B1_4.1']==0) & (df['B1_5.1']==0) & (df['B1_6.1']==0) & (df['B1_7.1']==0) & (df['B1_8.1']==0) & (df['B1_9.1']==0) & (df['B1_10.1']==0) & (df['B1_12.1']==0) & (df['B1_13.1']==0))])))
    print("Number of females " + str(len(df[df['E3.2'] == 1])))
    print("before removing Asymptomatic " + str(df.shape))
    df = df[(df['B1_1.1']==1) | (df['B1_2.1']==1) | (df['B1_3.1']==1) | (df['B1_4.1']==1) | (df['B1_5.1']==1) | (df['B1_6.1']==1) | (df['B1_7.1']==1) | (df['B1_8.1']==1) | (df['B1_9.1']==1) | (df['B1_10.1']==1) | (df['B1_12.1']==1) | (df['B1_13.1']==1)]
#     df2 = df1[df1['B8.NA']==0]
#     df3 = df2[df2['B8.3']==0]
    df3 = df
    print("after removing non tested and asymptomatic" + str(df3.shape))
    
    df3 = df3.assign(Cli=0)
    df3 = df3.assign(Cli_who=0)

    lista_valor_cli_DE =[]
    for i in range(len(df3.index)):
        if (df3['B1_1.1'][df3.index[i]] ==1 and (df3['B1_2.1'][df3.index[i]] ==1 or df3['B1_3.1'][df3.index[i]]==1)):
            lista_valor_cli_DE.append(1)
        else:
            lista_valor_cli_DE.append(0)


    df3['Cli']=lista_valor_cli_DE


    lista_valor_cli_who_DE =[]
    for i in range(len(df3.index)):
        if (df3['B1_1.1'][df3.index[i]] ==1 and df3['B1_2.1'][df3.index[i]] ==1 and df3['B1_4.1'][df3.index[i]]==1):
            lista_valor_cli_who_DE.append(1)
        else:
            lista_valor_cli_who_DE.append(0)
    df3['Cli_who']=lista_valor_cli_who_DE
    
    return df3


# In[8]:


#readRDS = robjects.r['readRDS']
#df_1 = readRDS("C:/Users/Administrator/Documents/Data_2021_dummies/2021-Q1/AU.rds")
#df_2 = readRDS("C:/Users/Administrator/Documents/Data_2021_dummies/2021-Q2/AU.rds")
#df_3 = readRDS("C:/Users/Administrator/Documents/Data_2021_dummies/2021-Q3/AU.rds")
#df_1 = pandas2ri.rpy2py_dataframe(df_1)
#df_2 = pandas2ri.rpy2py_dataframe(df_2)
#df_3 = pandas2ri.rpy2py_dataframe(df_3)
#df_try = [df_1,df_2]
#df_try = pd.DataFrame(pd.concat(df_try,join='inner', ignore_index=True))


# In[9]:


def filter_data_is(iso2): #,number
    #df = pd.read_csv(iso2+'_'+str(number)+'.csv')
    
    readRDS = robjects.r['readRDS']
    
    #My jupyter docker
    df_2 = readRDS("C:/Users/Administrator/Documents/Data_2021_dummies/2021-Q2/" + iso2 + ".rds")
    df_3 = readRDS("C:/Users/Administrator/Documents/Data_2021_dummies/2021-Q3/" + iso2 + ".rds")
    df_3 = df_3.head(5000)
    df_4 = readRDS("C:/Users/Administrator/Documents/Data_2021_dummies/2021-Q4/" + iso2 + ".rds")
    df_4 = df_4.head(5000)

    
#     print("2020 " + str(df_old.shape))
#     print("2021 " + str(df.shape))

    #Server 
    #df_2 = readRDS("/home/coronasurveys/UMD/2021-Q2/dummies/" + iso2 + ".rds")
    #df_3 = readRDS("/home/coronasurveys/UMD/2021-Q3/dummies/" + iso2 + ".rds")
    #df_4 = readRDS("/home/coronasurveys/UMD/2021-Q4/dummies/" + iso2 + ".rds")
    df_2 = pandas2ri.rpy2py_dataframe(df_2)
    df_3 = pandas2ri.rpy2py_dataframe(df_3)
    df_4 = pandas2ri.rpy2py_dataframe(df_4)
    
    #df_2 = df_2.loc["386842":"433987"] #from May 20 to the end of Q2, we have B15_2 in that span
    df_2 = df_2[(df_2['date'] > '2021-05-20')]
    df_total = [df_2,df_3,df_4]
    
    
    df = pd.DataFrame(pd.concat(df_total,join='inner', ignore_index=True)) 
    print("before removing non tested " + str(df.shape))
    df = df[(df['B7.1']==1) & (df['B8.NA']==0) & (df['B8.3']==0)]
    print("after removing non tested " + str(df.shape))
    print("Positives cases " + str(len(df[(df['B8.1']==1)])))
    print("Asymptomatic cases " + str(len(df[(df['B8.1']==1) & ((df['B1_1.1']==0) & (df['B1_2.1']==0) & (df['B1_3.1']==0) & (df['B1_4.1']==0) & (df['B1_5.1']==0) & (df['B1_6.1']==0) & (df['B1_7.1']==0) & (df['B1_8.1']==0) & (df['B1_9.1']==0) & (df['B1_10.1']==0) & (df['B1_12.1']==0) & (df['B1_13.1']==0))])))
    print("Number of females " + str(len(df[df['E3.2'] == 1])))
    print("before removing Asymptomatic " + str(df.shape))
    df = df[(df['B1_1.1']==1) | (df['B1_2.1']==1) | (df['B1_3.1']==1) | (df['B1_4.1']==1) | (df['B1_5.1']==1) | (df['B1_6.1']==1) | (df['B1_7.1']==1) | (df['B1_8.1']==1) | (df['B1_9.1']==1) | (df['B1_10.1']==1) | (df['B1_12.1']==1) | (df['B1_13.1']==1)]
#     df2 = df1[df1['B8.NA']==0]
#     df3 = df2[df2['B8.3']==0]
    df3 = df
    print("after removing non tested and asymptomatic" + str(df3.shape))
    
    df3 = df3.assign(Cli=0)
    df3 = df3.assign(Cli_who=0)

    lista_valor_cli_DE =[]
    for i in range(len(df3.index)):
        if (df3['B1_1.1'][df3.index[i]] ==1 and (df3['B1_2.1'][df3.index[i]] ==1 or df3['B1_3.1'][df3.index[i]]==1)):
            lista_valor_cli_DE.append(1)
        else:
            lista_valor_cli_DE.append(0)


    df3['Cli']=lista_valor_cli_DE


    lista_valor_cli_who_DE =[]
    for i in range(len(df3.index)):
        if (df3['B1_1.1'][df3.index[i]] ==1 and df3['B1_2.1'][df3.index[i]] ==1 and df3['B1_4.1'][df3.index[i]]==1):
            lista_valor_cli_who_DE.append(1)
        else:
            lista_valor_cli_who_DE.append(0)
    df3['Cli_who']=lista_valor_cli_who_DE
    
    return df3


# In[10]:


#df = filter_data_is("AU")


# In[11]:


def spanish_method(df_filter,y,sym = 4):
#     df = pd.read_csv('../data/'+iso2+'.csv')
#     df1 = df[df['B7.1']==1]
#     df2 = df1[df1['B8.NA']==0]
#     df_filter = df2[df2['B8.3']==0]
    
    df_filter = pd.concat([df_filter,y],axis = 1)
    column_drop = ["B1_1.NA","B1_2.NA","B1_3.NA","B1_4.NA","B1_7.NA","B1_10.NA","B1_12.NA","B1_13.NA"]
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    
    list_risk = []
    for i in range(len(df_filter.index)):
        risk= 1*df_filter['B1_4.1'][df_filter.index[i]] + df_filter['B1_7.2'][df_filter.index[i]] + 2*df_filter['B1_1.1'][df_filter.index[i]] + 5*df_filter['B1_10.1'][df_filter.index[i]]
        list_risk.append(risk)
    df_filter['risk']=list_risk
    
    
    list_sum_sym=[]
    for i in range(len(df_filter.index)):
        sum_sym= np.sum(df_filter['B1_4.1'][df_filter.index[i]]+df_filter['B1_10.1'][df_filter.index[i]]+df_filter['B1_3.1'][df_filter.index[i]]+df_filter['B1_13.1'][df_filter.index[i]]+df_filter['B1_1.1'][df_filter.index[i]]+df_filter['B1_7.2'][df_filter.index[i]]+df_filter['B1_2.1'][df_filter.index[i]]+df_filter['B1_12.1'][df_filter.index[i]])
        list_sum_sym.append(sum_sym)
    df_filter['sum_sym']=list_sum_sym
    

    list_sm = []
    for i in range(len(df_filter.index)):
        if (df_filter['risk'][df_filter.index[i]]>=3  and df_filter['sum_sym'][df_filter.index[i]]>=sym):
            list_sm.append(1)
        else:
            list_sm.append(0)
    print(df_filter.shape)
    print(len(list_sm))
    df_filter['spanish_method'] = list_sm

#     for i in range(len(df_filter.index)):
#         if df_filter['risk'][df_filter.index[i]]>=3:
#             if df_filter['sum_sym'][df_filter.index[i]]>=sym:
#                 list_sm.append(1)
#             else:
#                 list_sm.append(0)
#         else:
#             list_sm.append(0)
#     df_filter['spanish_method'] = list_sm

#     def f(df_filter):
#         if df_filter['risk'] >= 3 and df_filter['sum_sym'] >= sym:
#             list_sm = 1
#         else:
#             list_sm = 0
#         return list_sm
    
            
#     df_filter['spanish_method'] = df_filter.apply(f,axis = 1)
#     for i in range(len(df_filter.index)):
#         if (df_filter['risk'].iloc[i]>=3) & (df_filter['sum_sym'].iloc[i]>=sym):
#             list_sm.append(1)
#         else:
#             list_sm.append(0)
#     df_filter['spanish_method'] = list_sm

    #print(df_filter['spanish_method'])
    Y_test = df_filter['B8.1']
    y_pred = df_filter['spanish_method']

#     print(f'Accuracy Score: {accuracy_score(Y_test,y_pred)}')
#     print(f'Area Under Curve: {roc_auc_score(Y_test, y_pred)}')
#     print(f'Recall score: {recall_score(Y_test,y_pred)}')
#     print("")
#     matrix = confusion_matrix(Y_test, y_pred)
#     tn, fp, fn, tp = confusion_matrix(Y_test, y_pred).ravel()

#     print('Specificity:', tn/(tn+fp))
#     print(f'F1 score: {f1_score(Y_test,y_pred)}')
#     print(f'Precision score: {precision_score(Y_test,y_pred)}')
#     print(matrix)
#     print("")
#     print(classification_report(Y_test, y_pred))
#     plt.figure(figsize=(7,7))
#     sns.heatmap(matrix,annot=True, square = True, fmt='g')
#     plt.show()
    return Y_test,y_pred


# In[12]:


#X = Df_try_DE_1.drop(columns = ["B8.1"],axis=1)
#Y = Df_try_DE_1["B8.1"]
#train_x,test_x,train_y,test_y = train_test_split(X,Y,test_size = 0.20,random_state=2)
#Y_test,y_pred = spanish_method(test_x,test_y,4)
#result = save_result("Nombre",Y_test, y_pred)
#print(result)


# In[26]:


def Israel_preparation(X,Y):
    X = pd.DataFrame(X)
    X["age_65"] = X["E4.6"].copy() + X["E4.7"].copy()
    X["age_55"] = X["E4.5"].copy() + X["E4.6"].copy() + X["E4.7"].copy()
    Df_try_65 = X[["B1_2.1","B1_1.1","B1_7.1","B1_3.1","B1_12.1","age_65","E3.1","B15_2.1"]] 
    Df_try_55 = X[["B1_2.1","B1_1.1","B1_7.1","B1_3.1","B1_12.1","age_55","E3.1","B15_2.1"]]
    y_test = Y
    #Dropping NA
    Df_try_65  = pd.concat([pd.DataFrame(Df_try_65),Y,X["B1_2.NA"],X["B1_1.NA"],X["B1_7.NA"],X["B1_3.NA"],X["B1_12.NA"],X["E3.NA"]], axis=1)
    Df_try_55  = pd.concat([pd.DataFrame(Df_try_55),Y,X["B1_2.NA"],X["B1_1.NA"],X["B1_7.NA"],X["B1_3.NA"],X["B1_12.NA"],X["E3.NA"]], axis=1)
    Df_try_65_No_Na = Df_try_65.dropna()
    Df_try_55_No_Na = Df_try_55.dropna()
    column_drop = ["B1_2.NA","B1_1.NA","B1_7.NA","B1_3.NA","B1_12.NA","E3.NA"]
    for column in column_drop:
        Df_try_55_No_Na = Df_try_55_No_Na[Df_try_55_No_Na[column] != 1]
        Df_try_65_No_Na = Df_try_65_No_Na[Df_try_65_No_Na[column] != 1]
    y_test_55 = Df_try_55_No_Na["B8.1"]
    y_test_65 = Df_try_65_No_Na["B8.1"]
    Df_try_55 = Df_try_55.drop(columns=["B8.1","B1_2.NA","B1_1.NA","B1_7.NA","B1_3.NA","B1_12.NA","E3.NA"])
    Df_try_65 = Df_try_65.drop(columns=["B8.1","B1_2.NA","B1_1.NA","B1_7.NA","B1_3.NA","B1_12.NA","E3.NA"])
    Df_try_55_No_Na = Df_try_55_No_Na.drop(columns=["B8.1","B1_2.NA","B1_1.NA","B1_7.NA","B1_3.NA","B1_12.NA","E3.NA"])
    Df_try_65_No_Na = Df_try_65_No_Na.drop(columns=["B8.1","B1_2.NA","B1_1.NA","B1_7.NA","B1_3.NA","B1_12.NA","E3.NA"])
    return Df_try_55, Df_try_65, Df_try_55_No_Na, Df_try_65_No_Na, y_test_55, y_test_65, y_test


# In[14]:


def label_facebook (row):
    if ((row['E3.1'] == 1) & (row['young'] == 1)):
        return 'Male & young'
    if ((row['E3.1'] == 1) & (row['middle'] == 1)):
        return 'Male & middle'
    if ((row['E3.1'] == 1) & (row['elderly'] == 1)):
        return 'Male & elderly'
    if ((row['E3.2'] == 1) & (row['young'] == 1)):
        return 'Female & young'
    if ((row['E3.2'] == 1) & (row['middle'] == 1)):
        return 'Female & middle'
    if ((row['E3.2'] == 1) & (row['elderly'] == 1)):
        return 'Female & elderly'
    return 'Other'


# In[15]:


def Facebook_method(train_x,test_x,train_y,test_y):
    df_try = pd.concat([train_x,train_y],axis = 1)
    df_try["young"] = df_try["E4.1"] + df_try["E4.2"]
    df_try["middle"] = df_try["E4.3"] + df_try["E4.4"]
    df_try["elderly"] = df_try["E4.5"] + df_try["E4.6"] + df_try["E4.7"]
    df_try["Gender_Age"] = df_try.apply (lambda row: label_facebook(row), axis=1)
    print(set(df_try["Gender_Age"]))
    df_try_last = pd.DataFrame(pd.concat([df_try,pd.get_dummies(df_try["Gender_Age"])],axis = 1))
    #print(df_try_last.columns)
    df_try_last = df_try_last[['Male & young','Male & middle','Male & elderly','Female & young','Female & middle',
                             'Female & elderly','Other','B8.1','B1_1.1','B1_2.1','B1_3.1','B1_4.1','B1_5.1',
                              'B1_6.1','B1_7.1','B1_8.1','B1_9.1','B1_10.1','B1_12.1']]
    X_train = df_try_last.drop("B8.1", axis = 1)
    Y_train = df_try_last["B8.1"]
    model = lgb.LGBMClassifier(learning_rate=0.09,max_depth=-5,random_state=42)
    model = model.fit(X_train,Y_train)#,eval_set=[(x_test,y_test),(x_train,y_train)],eval_metric='logloss')
    #print("test x shape " + str(test_x.shape))
    test_x["young"] = test_x["E4.1"] + test_x["E4.2"]
    test_x["middle"] = test_x["E4.3"] + test_x["E4.4"]
    test_x["elderly"] = test_x["E4.5"] + test_x["E4.6"] + test_x["E4.7"]
    test_x["Gender_Age"] = test_x.apply (lambda row: label_facebook(row), axis=1)
    print(set(test_x["Gender_Age"]))
    test_x = pd.DataFrame(pd.concat([test_x.reset_index(drop=True),pd.get_dummies(test_x["Gender_Age"].reset_index(drop=True))],axis = 1))
    #print("test x shape now? " + str(test_x.shape))
    #print(df_try_last.columns)
    test_x = test_x[['Male & young','Male & middle','Male & elderly','Female & young','Female & middle',
                             'Female & elderly','Other','B1_1.1','B1_2.1','B1_3.1','B1_4.1','B1_5.1',
                              'B1_6.1','B1_7.1','B1_8.1','B1_9.1','B1_10.1','B1_12.1']]
    ypred = model.predict(test_x)
    return(test_y,ypred)


# In[16]:


def Other_research(train_x,test_x,train_y,test_y):
    print("\n############### COVID-19 symptoms and SARS-CoV-2 antibody positivity in a large survey of first responders and healthcare personnel ###############\n")
    print("\n---------------------- fever, shortness of breath (SOB) and chills ----------------------\n")
    #BETTER TO DO A LOGISTIC TREE WITH THOSE PARAMETERS? SOB = DIFFICULTY BREATHING
    column_drop = ["B1_1.NA","B1_3.NA","B1_13.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter['new'] = np.where(df_filter[['B1_1.1','B1_3.1','B1_13.1']].sum(axis=1).eq(3),1,0)
    #evaluation_fun(test_y,df_filter['new'])
    result = save_result("Akinbami_1",df_filter['B8.1'] ,df_filter['new'])
    result = result.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 

    print("\n---------------------- fever, shortness of breath (SOB) and loss of taste/smell ----------------------\n")
    column_drop = ["B1_1.NA","B1_3.NA","B1_10.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter['new'] = np.where(df_filter[['B1_1.1','B1_3.1','B1_10.1']].sum(axis=1).eq(3),1,0)
    #evaluation_fun(test_y,df_filter['new'])
    result_new = save_result("Akinbami_2",df_filter['B8.1'],df_filter['new'])
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)

    print("\n---------------------- fever, shortness of breath (SOB), chills and headache ----------------------\n")
    column_drop = ["B1_1.NA","B1_3.NA","B1_13.NA","B1_12.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter['new'] = np.where(df_filter[['B1_1.1','B1_3.1','B1_13.1','B1_12.1']].sum(axis=1).eq(3),1,0)
    #evaluation_fun(test_y,df_filter['new'])
    result_new = save_result("Akinbami_3",df_filter['B8.1'],df_filter['new'])
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### Real-time tracking of self-reported symptoms to predict potential COVID-19 USE \n")
    #BETTER TO DO A LOGISTIC REGRESSION WITH THOSE PARAMETERS?
    print("\n---------------------- Method 1 ----------------------\n")
    column_drop = ["E4.NA","E3.NA","B1_10.NA","B1_4.NA","B1_2.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter['new'] = -1.32 - 0.01 * 21 * df_filter["E4.1"] - 0.01 * 29.5 * df_filter["E4.2"] - 0.01 * 39.5 * df_filter["E4.3"] - 0.01 * 49.5 * df_filter["E4.4"] - 0.01 * 59.5 * df_filter["E4.5"] - 0.01 * 69.5 * df_filter["E4.6"] - 0.01 * 80 * df_filter["E4.7"] + 0.44 * df_filter["E3.1"] + 1.75 * df_filter["B1_10.1"] + 0.31 * df_filter["B1_2.1"] + 0.49 * df_filter["B1_4.1"]
    df_filter['new'] = np.exp(df_filter['new'])/(1+np.exp(df_filter['new']))
    a = np.array(df_filter['new'].values.tolist())
    df_filter['new'] = np.where(a > 0.5, 1, 0).tolist() #Check
    ypred = df_filter['new']
    #evaluation_fun(test_y,ypred)
    result_new = save_result("Menni_1",df_filter['B8.1'],ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)

    print("\n---------------------- Method 2 ----------------------\n")
    column_drop = ["E4.NA","E3.NA","B1_10.NA","B1_4.NA","B1_2.NA"]
    df_filter = pd.concat([train_x,train_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter = df_filter[["E4.1","E4.2","E4.3","E4.4","E4.5","E4.6","E4.7","E3.1","B1_10.1","B1_4.1","B1_2.1","B8.1"]] #
    test_x_now = test_x[["E4.1","E4.2","E4.3","E4.4","E4.5","E4.6","E4.7","E3.1","B1_10.1","B1_4.1","B1_2.1"]]
    LR = LogisticRegression(random_state=2, max_iter = 1000,class_weight='balanced')
    LR = LR.fit(df_filter.drop(["B8.1"],1),df_filter['B8.1'])
    ypred = LR.predict(test_x_now)
    #evaluation_fun(test_y,ypred)
    result_new = save_result("Menni_2",test_y,ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### A Symptom-Based Rule for Diagnosis of COVID-19 ###############\n")
    # PHENOTYPE? 
    print("\n---------------------- Method 1 ----------------------\n")
    column_drop = ["B1_1.NA","B1_2.NA","B1_10.NA","B1_8.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter['new'] = 2*df_filter["B1_10.1"] + df_filter["B1_1.1"] + df_filter["B1_2.1"] - df_filter["B1_8.1"]
    df_filter['new'] = np.exp(df_filter['new'])/(1+np.exp(df_filter['new']))
    f1_score, threshold, ypred = Best_f1_score(df_filter['new'],df_filter['B8.1'])
    #evaluation_fun(test_y,ypred)
    result_new = save_result("Smith_1",df_filter['B8.1'],ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n---------------------- Method 2 ----------------------\n")
    df_filter = df_filter[["B1_10.1","B1_1.1","B1_2.1","B1_8.1","B8.1"]] #
    test_x_now = test_x[["B1_10.1","B1_1.1","B1_2.1","B1_8.1"]]
    LR = LogisticRegression(random_state=2, max_iter = 1000,class_weight='balanced')
    LR = LR.fit(df_filter.drop(["B8.1"],1),df_filter['B8.1'])
    ypred = LR.predict(test_x_now)
    #evaluation_fun(test_y,ypred)
    result_new = save_result("Smith_2",test_y,ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### Anticipating the curve: can online symptom-based data reflect COVID-19 case activity in Ontario, Canada? ###############\n")
    #RIGORS? NOT IN THE SURVEY, SOB = DIFFICULTY BREATHING
    column_drop = ["B1_1.NA","B1_13.NA","B1_6.NA","B1_12.NA","B1_7.NA","B1_2.NA","B1_3.NA","B1_10.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    Cond_1 = df_filter["B1_1.1"] + df_filter["B1_13.1"] + df_filter["B1_6.1"] + df_filter["B1_12.1"] + df_filter["B1_7.1"]
    Cond_2 = df_filter["B1_2.1"] + df_filter["B1_3.1"] + df_filter["B1_10.1"]
    df_cond = pd.concat([Cond_1, Cond_2], axis=1)
    df_cond = df_cond.rename(columns={0: "Cond_1", 1: "Cond_2"})
    ypred = []
    for i in range(len(df_filter.index)):
        if (df_cond['Cond_1'][df_filter.index[i]]>=2  or df_cond['Cond_2'][df_filter.index[i]]>=1):
            ypred.append(1)
        else:
            ypred.append(0)
    #evaluation_fun(df_filter['B8.1'],ypred)
    result_new = save_result("Arjuna",df_filter['B8.1'],ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### Smell and taste symptom-based predictive model for COVID-19 diagnosis ###############\n")
    #USING LR =? binary logistic regression
    column_drop = ["B1_10.NA","B1_6.NA","B1_1.NA","B1_3.NA","B1_7.NA"] #,"B1_13.NA"
    df_filter = pd.concat([train_x,train_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter = df_filter[["B1_10.1","B1_6.1","B1_1.1","B1_3.1","B1_7.1","B8.1"]]
    test_x_now = test_x[["B1_10.1","B1_6.1","B1_1.1","B1_3.1","B1_7.1"]]
    LR = LogisticRegression(random_state=2, max_iter = 1000,class_weight='balanced' )
    LR = LR.fit(df_filter.drop(["B8.1"],axis = 1),df_filter['B8.1'])
    ypred = LR.predict(test_x_now)
    #evaluation_fun(y_test,ypred)
    result_new = save_result("Ronald",test_y,ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### Development and Validation of a Clinical Symptom-based Scoring System for Diagnostic Evaluation of COVID-19 Patients Presenting to Outpatient Department in a Pandemic Situation ###############n")
    #BETTER TO DO A LOGISTIC REGRESSION WITH THOSE FEATURES?
    column_drop = ["B1_1.NA","B1_2.NA","B1_6.NA","B1_12.NA","B1_10.NA"]
    df_filter = pd.concat([test_x,test_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter['new'] = 41.7 * df_filter["B1_1.1"] + 13.5 * df_filter["B1_2.1"] + 15.8 * df_filter["B1_12.1"] + 10 * df_filter["B1_6.1"] + 94.7 * df_filter["B1_10.1"]
    df_filter['new'] = np.where(df_filter['new'] > 41.7, 1, 0)

    #evaluation_fun(test_y,df_filter['new'])
    result_new = save_result("Bhattacharya",df_filter['B8.1'],df_filter['new'])
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### Symptom-based early-stage differentiation between SARS-CoV-2 versus other respiratory tract infections Upper Silesia pilot study #\n")
    #print("SOMETHING IS WRONG WITH THIS MODEL")
    # B1_9 NOT GI (assumption)
    column_drop = ["B1_10.NA","B1_2.NA","B1_1.NA","B1_9.NA"]
    df_filter = pd.concat([train_x,train_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter = df_filter[["B1_10.1","B1_2.1","B1_1.1","B1_9.1","B8.1"]] #
    test_x_new =test_x[["B1_10.1","B1_2.1","B1_1.1","B1_9.1"]]
    LR = LogisticRegression(random_state=2, max_iter = 1000,class_weight='balanced')
    LR = LR.fit(df_filter.drop(["B8.1"],1),df_filter['B8.1'])
    ypred = LR.predict(test_x_new)
    #evaluation_fun(test_y,ypred)
    result_new = save_result("Mika",test_y,ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    print("\n############### Who should we test for COVID-19? A triage model built from national symptom surveys ###############\n")
    # DIFFICULTY BREATHING NOT shortness of breath (assumption)
    column_drop = ["B1_10.NA","B1_2.NA","B1_1.NA","B1_3.NA","B1_7.NA","E3.NA","E4.NA"] 
    df_filter = pd.concat([train_x,train_y],axis = 1)
    for column in column_drop:
        df_filter = df_filter[df_filter[column] != 1]
    df_filter = df_filter[["B1_10.1","B1_2.1","B1_1.1","B1_3.1","B1_7.1","E3.1","E4.1","E4.2","E4.3","E4.4","E4.5","E4.6","E4.7","B8.1"]] #
    test_x_new =test_x[["B1_10.1","B1_2.1","B1_1.1","B1_3.1","B1_7.1","E3.1","E4.1","E4.2","E4.3","E4.4","E4.5","E4.6","E4.7"]]
    LR = LogisticRegression(random_state=2, max_iter = 1000,class_weight='balanced')
    LR = LR.fit(df_filter.drop(["B8.1"],1),df_filter['B8.1'])
    ypred = LR.predict(test_x_new)
    #evaluation_fun(test_y,ypred)
    result_new = save_result("Shoer",test_y,ypred)
    result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"}) 
    result = pd.concat([result, result_new], axis=1)
    
    return result


# In[27]:


def Boostraping_others_methods(df,n_iterations):
    # configure bootstrap
    # run bootstrap
    AUC_cli, AUC_cli_who, AUC_cli_Spain, AUC_cli_Israel_55_NA_DE, AUC_cli_Israel_65_NA_DE = list(),list(),list(),list(),list()
    F1_score_cli, F1_score_cli_who, F1_score_Spain, F1_score_Israel_55_NA_DE, F1_score_Israel_65_NA_DE = list(),list(),list(),list(),list()
    Specificity_cli, Specificity_cli_who, Specificity_Spain, Specificity_Israel_55_NA_DE, Specificity_Israel_65_NA_DE = list(),list(),list(),list(),list()
    Sensitivity_cli, Sensitivity_cli_who, Sensitivity_Spain, Sensitivity_Israel_55_NA_DE, Sensitivity_Israel_65_NA_DE = list(),list(),list(),list(),list()
    data_final = pd.DataFrame()
    for i in range(n_iterations):
        if(i == 100 |i == 200 |i == 300 |i == 400):
            print(i)
        df_now = df.sample(n = df.shape[0] ,replace = True) #For Bootstrap
        
        X = df.drop(columns = ["B8.1"],axis=1)
        Y = df["B8.1"]
        train_x,test_x,train_y,test_y = train_test_split(X,Y,test_size = 0.20,random_state=i)
        
#         X_is = df_is.drop(columns = ["B8.1"],axis=1)
#         Y_is = df_is["B8.1"]
#         train_x_is,test_x_is,train_y_is,test_y_is = train_test_split(X_is,Y_is,test_size = 0.20,random_state=i)
        
        data_eval = pd.DataFrame()
        
        #Cli
        data_new = save_result("CLI",test_y ,test_x["Cli"]) 
        data_new = data_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        data_eval = pd.concat([data_eval,data_new])
        
        #Cli_who
        data_new = save_result("CLI_WHO",test_y,test_x["Cli_who"])
        data_new = data_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        data_eval = pd.concat([data_eval,data_new], axis=1)
        
        #Israel
        #modelo=lgb.Booster(model_file = "/home/jrufino/comparing_models_paper/lgbm_model_all_features.txt")
        #modelo_1=lgb.Booster(model_file = "/home/jrufino/comparing_models_paper/lgbm_model_balanced_features.txt")
        #modelo=lgb.Booster(model_file = "C:/Users/Administrator/Documents/Comparing_paper/lgbm_model_all_features.txt")
        #modelo_1=lgb.Booster(model_file = "C:/Users/Administrator/Documents/Comparing_paper/lgbm_model_balanced_features.txt")
        #Df_try_55, Df_try_65, Df_try_55_No_Na, Df_try_65_No_Na,y_test_55, y_test_65, y_test = Israel_preparation(test_x,test_y) 
    
        #With age above 55
        #ypred_55 = modelo.predict(Df_try_55)
        #f1_score_fin, threshold, ypred_55_fin = Best_f1_score(ypred_55,y_test) #NO SE DEBERÍA HACE ESTO ANTES DE SACAR AUC
        #data_new = save_result(" Zoabi_55",y_test,ypred_55_fin)
        #data_new = data_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        #data_eval = pd.concat([data_eval,data_new], axis=1)

        #With age above 65
        #ypred_65 = modelo.predict(Df_try_65)
        #f1_score_fin, threshold, ypred_65_fin = Best_f1_score(ypred_65,y_test) #NO SE DEBERÍA HACE ESTO ANTES DE SACAR AUC
        #data_new = save_result("Zoabi_65",y_test,ypred_65_fin)
        #data_new = data_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        #data_eval = pd.concat([data_eval,data_new], axis=1)
        
        #Spain
        Y_test,y_pred = spanish_method(test_x,test_y,4)
        data_new = save_result("Perez",Y_test,y_pred)
        data_new = data_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        data_eval = pd.concat([data_eval,data_new], axis=1)
        
        #Facebook
        Y_test,y_pred = Facebook_method(train_x,test_x,train_y,test_y)
        data_new = save_result("Facebook",Y_test,y_pred)
        data_new = data_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        data_eval = pd.concat([data_eval,data_new], axis=1)

        #Other studies
        result_new = Other_research(train_x,test_x,train_y,test_y) #Cambiar por X e Y
        result_new = result_new.rename(index={0: "AUC", 1: "F1_score", 2:"Specificity", 3: "Sensitivity"})
        data_eval = pd.concat([data_eval.reset_index(drop=True), result_new.reset_index(drop=True)], axis=1)#.reset_index(drop=True)
        data_final = pd.concat([data_final,data_eval])
        
        
    
    return data_final           


# ## MAIN

# In[18]:


#Df_try_DE_1 = filter_data ("DE")
#cols_to_drop = [col for col in Df_try_DE_1.columns if ("NA" in col)]
#cols_to_drop_2 = ["B8.2","B8.3","Finished","weight","B4",'E6', 'survey_version',"ISO_3","country_agg","B2","E5","region_agg","date_from_file","age","ISO2","date","RecordedDate","Cli","Cli_who"]
#cols_to_drop_3 = ["B8.2","B8.3","Finished","weight","B4",'E6', 'survey_version',"ISO_3","country_agg","B2","E5","region_agg","date_from_file","age","ISO2","date","RecordedDate"]
#Df_try_DE = Df_try_DE_1.drop(cols_to_drop + cols_to_drop_2,1)
#Df_try_DE_1 = Df_try_DE_1.drop(cols_to_drop_3,1)
#Df_try_ES_1 = filter_data ("ES")
#Df_try_ES = Df_try_ES_1.drop(cols_to_drop + cols_to_drop_2,1)
#Df_try_ES_1 = Df_try_ES_1.drop(cols_to_drop_3,1)
#Df_try_BR_1 = filter_data ("BR")
#Df_try_BR = Df_try_BR_1.drop(cols_to_drop + cols_to_drop_2,1)
#Df_try_BR_1 = Df_try_BR_1.drop(cols_to_drop_3,1)
#Df_try_IN_1 = filter_data ("IN")
#Df_try_IN = Df_try_IN_1.drop(cols_to_drop + cols_to_drop_2,1)
#Df_try_IN_1 = Df_try_IN_1.drop(cols_to_drop_3,1)


# In[28]:


countries = ("BR","CA","DE","JP","ZA") 
n_iterations = 100
for i in countries:
    print("Procesing " + str(i))
    Df_try_DE_1 = filter_data (i)
    cols_to_drop = [col for col in Df_try_DE_1.columns if ("NA" in col)]
    cols_to_drop_3 = ["B8.2","B8.3","Finished","weight","B4",'E6', 'survey_version',"ISO_3","country_agg","B2","E5","region_agg","date_from_file","age","ISO2","date","RecordedDate"]
    Df_try_DE_1 = Df_try_DE_1.drop(cols_to_drop_3,1)
#     Df_try_is = filter_data_is (i)
#     Df_try_is = Df_try_is.drop(cols_to_drop_3,1)
    df_DE_other_Metrics = Boostraping_others_methods(Df_try_DE_1,n_iterations) #Df_try_is
    if(i == "AU"):
        pd.options.display.max_columns = None
        #print(df_DE_other_Metrics.head())
        df_DE_other_Metrics.to_csv("df_AU_other_Metrics_CI_Symp_fin_2020.csv")
        print("Fin " + str(i))
    elif(i == "CA"):
        df_DE_other_Metrics.to_csv("df_CA_other_Metrics_CI_Symp_fin_2020.csv")
        print("Fin " + str(i))
    elif(i == "DE"):
        df_DE_other_Metrics.to_csv("df_DE_other_Metrics_CI_Symp_fin_2020.csv")
        print("Fin " + str(i))
    elif(i == "JP"):
        df_DE_other_Metrics.to_csv("df_JP_other_Metrics_CI_Symp_fin_2020.csv")
        print("Fin " + str(i))
    elif(i == "ZA"):
        df_DE_other_Metrics.to_csv("df_ZA_other_Metrics_CI_Symp_fin_2020.csv")
    elif(i == "BR"):
        df_DE_other_Metrics.to_csv("df_BR_other_Metrics_CI_Symp_fin_2020.csv")

#         print("Fin " + str(i))


# In[21]:


#Df_try_DE_1 = filter_data ("AU")
#cols_to_drop = [col for col in Df_try_DE_1.columns if ("NA" in col)]
#cols_to_drop_3 = ["B8.2","B8.3","Finished","weight","B4",'E6', 'survey_version',"ISO_3","country_agg","B2","E5","region_agg","date_from_file","age","ISO2","date","RecordedDate"]
#Df_try_DE_1 = Df_try_DE_1.drop(cols_to_drop_3,1)
#Df_try_is = filter_data_is ("AU")
#Df_try_is = Df_try_is.drop(cols_to_drop_3,1)
#df_DE_other_Metrics = Boostraping_others_methods(Df_try_DE_1,Df_try_is,10)
#df_DE_other_Metrics.head()


# In[ ]:


#n_iterations = 100
#df_DE_other_Metrics = Boostraping_others_methods(Df_try_DE_1,n_iterations)
#df_DE_other_Metrics.head()
#df_DE_other_Metrics.to_csv("df_DE_other_Metrics_CI_Symp_fin.csv")
#df_IN_other_Metrics = Boostraping_others_methods(Df_try_IN_1,n_iterations)
#df_IN_other_Metrics.to_csv("df_IN_other_Metrics_CI_Symp_fin.csv")
#df_ES_other_Metrics = Boostraping_others_methods(Df_try_ES_1,n_iterations)
#df_ES_other_Metrics.to_csv("df_ES_other_Metrics_CI_Symp_fin.csv")

