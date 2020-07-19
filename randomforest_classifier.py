#!/usr/bin/env python
# coding: utf-8
# python 3.6

# Random forest regressor predicting cart status
# Variables: price(replace real with skuPrice), sku, dept, hod, dow, ispopsku, ispopdept, duration longer than 30 sec

# Packages used: pandas, numpy, sklearn
# pip3 install pandas
# pip3 install numpy
# pip3 install sklearn

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import joblib
from sklearn import preprocessing
from sklearn.ensemble import RandomForestRegressor


# Export classification features and status from database to appAug.csv file
df = pd.read_csv("appAug.csv",dtype={0:str, 1:int, 2:int, 3:int, 4:int, 5:int, 6:float, 7:float, 8:float} ,sep="|",header =None)
df = df.rename(columns={0: "status",1: "sku", 2: "dept", 3:"hod",4:"dow",5:"dT", 6:"skuPrice",7:"total", 8:"freight"})
df.shape

# replace na with skuPrice info
df['total'] = df['total'].fillna(0)
hasReal = df['total'] > 0
df['price'] = df['skuPrice']*(1-hasReal) + df['total']*hasReal

# Add: popsku, popdept, which are popular sku ids and department ids across all carts in the database
# Export skuid ordered by the count of carts descending from database to popSku.csv file
popsku = pd.read_csv("popSku.csv",dtype={0:int} ,sep="|",header =None)
popsku = popsku.rename(columns={0: "sku"})

# Export deptid ordered by the count of carts descending from database to popdept.csv file
popdept = pd.read_csv("popdept.csv",dtype={0:int} ,sep="|",header =None)
popdept = popdept.rename(columns={0: "dept"})

# Create features ispopsku and ispopdept
df['ispopsku'] = df['sku'] == popsku['sku'][0]
df['ispopdept'] = df['dept'] == popdept['dept'][0]

for i in range(9):
    df['ispopsku'] = (df['ispopsku'] == 1) | (df['sku'] == popsku['sku'][i+1])
    df['ispopdept'] = (df['ispopdept'] == 1) | (df['dept'] == popdept['dept'][i+1])

# Convert to category type to improve memory usage
df["status"] = df["status"].astype('category')
df["sku"] = df["sku"].astype('category')
df["dept"] = df["dept"].astype('category')

# preprocessing - label encode skuid, deptid
le = preprocessing.LabelEncoder()

df['sku'] = le.fit_transform(df['sku'])
df['dept'] = le.fit_transform(df['dept'])

# Add in dt30 as variable for duration longer than 30 seconds
df['dt30'] = df['dT'] > 30

# Splitting training testing set - 80:20 ratio
train = df.sample(frac=0.8,random_state=200) #random state is a seed value
test = df.drop(train.index)

# Onehot encoding - return number of columns & encoded status
from sklearn.preprocessing import OneHotEncoder

def oneHot(y):
    drop_binary_enc = OneHotEncoder(handle_unknown='ignore').fit(y)
    y_encode = drop_binary_enc.transform(y).toarray()
    n = y_encode.shape[0]
    return n, y_encode

# Onehot encode status - target variable for training and testing set
y_train = train.iloc[:,0:1]
y_train = oneHot(y_train)[1]

y_test = test.iloc[:,0:1]
y_test = oneHot(y_test)[1]

print(y_train.shape)
print(y_test.shape)

# Generate prediction
# Input: regressor and X variables of test set
def pred(regressor, X_test):
    y_pred_can = regressor[0].predict(X_test)
    y_pred_exp = regressor[1].predict(X_test)
    y_pred_pur = regressor[2].predict(X_test)

    y_pred = [y_pred_can, y_pred_exp, y_pred_pur]
    return y_pred

# plot precision and recall for each status
# plot roc curve for each status with auc
# input: y_pred, y_test, name of plot
# output: prints the ROC curves and save to name.png

from sklearn.metrics import roc_curve
from sklearn.metrics import auc

def plotRoc(y_pred, y_test, name):
    fig, ax = plt.subplots(figsize=(13,13))
    ax = plt.axes()
    fig.suptitle('Random Forest Prediction', fontsize=40, fontweight='bold')

    ax.set_title('Variables: Longer30Sec, price, sku, hod, dow, dept', fontsize = 30)

    fpr_can, tpr_can, _ = roc_curve(y_test[:,0], y_pred[0])
    fpr_exp, tpr_exp, _ = roc_curve(y_test[:,1], y_pred[1])
    fpr_pur, tpr_pur, _ = roc_curve(y_test[:,2], y_pred[2])

    ax.plot(fpr_pur, tpr_pur, label='Purchased (AUC = %0.3f)' % auc(fpr_pur, tpr_pur),linewidth=6, markersize=16)
    ax.plot(fpr_can, tpr_can, label='Canceled (AUC = %0.3f)' % auc(fpr_can, tpr_can),linewidth=4, markersize=14)
    ax.plot(fpr_exp, tpr_exp, label='Expired (AUC = %0.3f)' % auc(fpr_exp, tpr_exp),linewidth=2, markersize=12)

    ax.legend(loc="lower right", fontsize = 37.8)

    ax.tick_params(labelsize=40, width=3)
    ax.grid(True)
    ax.set_xlabel('False Positive Rate', fontsize = 40.0)
    ax.set_ylabel('True Positive Rate', fontsize = 40.0)
    fig.savefig(name + 'roc_curve.png')
    return fpr_can, tpr_can, fpr_exp, tpr_exp, fpr_pur, tpr_pur

# plot feature importance of the regressors
# input: X variable names, feature importance, name of plot
# output: prints the feature importance plots and save to name.png

def featImp(XVar, importance, name):
    status = ['CANCELED', 'EXPIRED', 'PURCHASED']

    for i in range(len(importance)):
        importances = importance[i]
        importance.append(importances)
        indices = np.argsort(importances)
        fig, axs = plt.subplots(figsize=(25,25))

        axs.set_title(status[i] + ' Feature Importance', fontsize=70, fontweight='bold')
        axs.barh(range(len(indices)), importances[indices], align='center')
        axs.set_xlabel('Average Decrease in Impurity over Trees', fontsize = 50.0, fontweight='bold')
        axs.set_yticks(range(len(indices)))
        axs.set_yticklabels([XVar[j] for j in indices],fontdict = {'fontsize':60})

        axs.tick_params(labelsize=60, width=3)

        fig.savefig(name+'featImp'+'_'+status[i]+'.png')

# prepare X variables, drop unuseful features
X_train = train.drop(columns=['status', 'total', 'freight', 'skuPrice', 'dT'])
X_test = test.drop(columns=['status', 'total', 'freight', 'skuPrice', 'dT'])

# initialize variables
threshold = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]
trees = [50]
XVar = ['sku', 'dept', 'hod', 'dow',  'price', 'pop'+'\n'+'sku', 'pop'+'\n'+'dept', 'Longer'+'\n'+'30Sec']

# training random forest regressor on 3 statuses
for i in range(len(trees)):
    regressor1 = RandomForestRegressor(n_estimators=trees[i], random_state=0)
    regressor2 = RandomForestRegressor(n_estimators=trees[i], random_state=0)
    regressor3 = RandomForestRegressor(n_estimators=trees[i], random_state=0)

    regressor = [regressor1, regressor2, regressor3]
    regressor[0].fit(X_train, y_train[:,0])
    regressor[1].fit(X_train, y_train[:,1])
    regressor[2].fit(X_train, y_train[:,2])

    y_pred = pred(regressor, X_test)

    importance = []
    for j in range(len(regressor)):
        imp = regressor[j].feature_importances_
        importance.append(imp)
    joblib.dump(y_pred, "pred_"+str(trees[i])+".sav")
    joblib.dump(importance, "featImp_"+str(trees[i])+".sav")

    plotRoc(y_pred, y_test, str(trees[i])+'_trees_')
    featImp(XVar, importance, str(trees[i])+'_trees_')
