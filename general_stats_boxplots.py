#!/usr/bin/env python
# coding: utf-8

# # General Boxplots
# Total price breakdown by status
# Duration breakdown by status

# Packages used: pandas, numpy, sklearn
# pip3 install pandas
# pip3 install numpy
# pip3 install sklearn

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# select s2, dT from tmp_deltaT t, tmp_status_time s where t.id=s.id;
df = pd.read_csv("deltaT.csv",dtype={0:str, 1:int} ,sep="|",header =None)
df = df.rename(columns={0: "status",1: "dt"})

df.shape

pur = df[df['status']=='PURCHASED']['dt']/60
can = df[df['status']=='CANCELED']['dt']/60
exp = df[df['status']=='EXPIRED']['dt']/60

# Calculating median of duration of each status
print(np.median(pur))
print(np.median(can))
print(np.median(exp))

# Calculate average duration of each status
print(np.mean(pur))
print(np.mean(can))
print(np.mean(exp))

# Calculate the average of duration without the outliers
q1p = np.quantile(pur,.25)
q3p = np.quantile(pur,.75)
iqrp = q3p - q1p
print(np.mean(pur * ((pur > q1p-1.5*iqrp) & (pur < q3p+1.5*iqrp))))
q1c = np.quantile(can,.25)
q3c = np.quantile(can,.75)
iqrc = q3c - q1c
print(np.mean(can * ((can > q1c-1.5*iqrc) & (can < q3c+1.5*iqrc))))
q1e = np.quantile(exp,.25)
q3e = np.quantile(exp,.75)
iqre = q3e - q1e
print(np.mean(exp * ((exp > q1e-1.5*iqre) & (exp < q3e+1.5*iqre))))



dtdata = [pur,can,exp]
# Plot the boxplots of duration for each status
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart checkout duration broken down by status', fontsize=35, fontweight='bold')

boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')

bp = axs.boxplot(dtdata, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Duration (min)', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)
fig.savefig('cart_time_box.png')

# Get data for price information
# select s2, dT from tmp_deltaT t, tmp_status_time s where t.id=s.id;
df2 = pd.read_csv("total.csv",dtype={0:str, 1:float} ,sep="|",header =None)
df2 = df2.rename(columns={0: "status",1: "total"})

# Separate into each status
# Convert original currency to US dollar
pur2 = df2[df2['status']=='PURCHASED']['total']/1.00 #actual value redacted to avoid compromising anonymity
can2 = df2[df2['status']=='CANCELED']['total']/1.00 #actual value redacted to avoid compromising anonymity
exp2 = df2[df2['status']=='EXPIRED']['total']/1.00 #actual value redacted to avoid compromising anonymity
pdata = [pur2, can2, exp2]

# Boxplots of Real price broken down by status
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart total price broken down by status', fontsize=35, fontweight='bold')

boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
# axs.set_title('Breakdown by Status', fontsize = 35.0)
bp = axs.boxplot(pdata, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Total Price ($USD) ', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)

fig.savefig('cart_price1_box.png')

# select s2, dT from tmp_deltaT t, tmp_status_time s where t.id=s.id;
df3 = pd.read_csv("skuprice.csv",dtype={0:float, 1:float, 2:float, 3:str} ,sep="|",header =None)
df3 = df3.rename(columns={0: "skuPrice",1: "total",2: "freight",3: "status"})

# replace na real price with skuPrice
df3['total'] = df3['total'].fillna(0)
hasReal = df3['total'] > 0
df3['price'] = df3['skuPrice']*(1-hasReal) + df3['total']*hasReal

pur3 = df3[df3['status']=='PURCHASED']['price']/5.36
can3 = df3[df3['status']=='CANCELED']['price']/5.36
exp3 = df3[df3['status']=='EXPIRED']['price']/5.36

skudata = [pur3, can3, exp3]

# Calculate median of replaced price
print(np.median(pur3))
print(np.median(can3))
print(np.median(exp3))

df3[df3['freight']>0].shape[0]/df3.shape[0]
df3.shape

withfreight = df3[(df3['freight']>0)]
pur4 = withfreight[withfreight['status']=='PURCHASED']['freight']/5.36
can4 = withfreight[withfreight['status']=='CANCELED']['freight']/5.36
exp4 = withfreight[withfreight['status']=='EXPIRED']['freight']/5.36
freight = [pur4, can4, exp4]

# Boxplots of freight price broken down by status
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart price broken down by status', fontsize=35, fontweight='bold')

boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
bp = axs.boxplot(skudata, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Price ($USD)', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)
fig.savefig('cart_price2_box.png')

# Boxplots of price broken down by status, replacing na real price with skuPrice
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart shipping price broken down by status', fontsize=35, fontweight='bold')

boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
bp = axs.boxplot(freight, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Shipping Price ($USD)', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)
fig.savefig('cart_shipping_box.png')
