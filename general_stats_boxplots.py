#!/usr/bin/env python
# coding: utf-8
# python 3.6

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

# Export duration and status from database to deltaT.csv file
df = pd.read_csv("deltaT.csv",dtype={0:str, 1:int} ,sep="|",header =None)
df = df.rename(columns={0: "status",1: "dt"})

# Group carts by status and convert duration from seconds to minutes
pur = df[df['status']=='PURCHASED']['dt']/60
can = df[df['status']=='CANCELED']['dt']/60
exp = df[df['status']=='EXPIRED']['dt']/60

# Calculate median of duration of each status
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
# Set Plot size and title
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart checkout duration broken down by status', fontsize=35, fontweight='bold')
# Set line styles
boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
# Boxplot without showing outliers
bp = axs.boxplot(dtdata, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Duration (min)', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)
fig.savefig('cart_time_box.png')

# Export duration and status from database to total.csv file
df2 = pd.read_csv("total.csv",dtype={0:str, 1:float} ,sep="|",header =None)
df2 = df2.rename(columns={0: "status",1: "total"})

# Separate into each status
# Convert original currency to US dollar
pur2 = df2[df2['status']=='PURCHASED']['total']/1.00 #actual value redacted to avoid compromising anonymity
can2 = df2[df2['status']=='CANCELED']['total']/1.00 #actual value redacted to avoid compromising anonymity
exp2 = df2[df2['status']=='EXPIRED']['total']/1.00 #actual value redacted to avoid compromising anonymity
pdata = [pur2, can2, exp2]

# Boxplots of Real price broken down by status
# Set Plot size and title
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart total price broken down by status', fontsize=35, fontweight='bold')
# Set line styles
boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
# Boxplot
bp = axs.boxplot(pdata, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Total Price ($USD) ', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)

fig.savefig('cart_price1_box.png')

# Export skuprice, total, shipping price and status from database to skuprice.csv file
df3 = pd.read_csv("skuprice.csv",dtype={0:float, 1:float, 2:float, 3:str} ,sep="|",header =None)
df3 = df3.rename(columns={0: "skuPrice",1: "total",2: "freight",3: "status"})

# replace na real price with skuPrice
df3['total'] = df3['total'].fillna(0)
hasReal = df3['total'] > 0
df3['price'] = df3['skuPrice']*(1-hasReal) + df3['total']*hasReal

pur3 = df3[df3['status']=='PURCHASED']['price']/1.00 #actual value redacted to avoid compromising anonymity
can3 = df3[df3['status']=='CANCELED']['price']/1.00 #actual value redacted to avoid compromising anonymity
exp3 = df3[df3['status']=='EXPIRED']['price']/1.00 #actual value redacted to avoid compromising anonymity

skudata = [pur3, can3, exp3]

# Calculate median of replaced price
print(np.median(pur3))
print(np.median(can3))
print(np.median(exp3))

# Percentage of carts with shipping price information out of the total number of carts
df3[df3['freight']>0].shape[0]/df3.shape[0]
df3.shape

# Filter data to get carts which have shipping price information
withfreight = df3[(df3['freight']>0)]
pur4 = withfreight[withfreight['status']=='PURCHASED']['freight']/1.00 #actual value redacted to avoid compromising anonymity
can4 = withfreight[withfreight['status']=='CANCELED']['freight']/1.00 #actual value redacted to avoid compromising anonymity
exp4 = withfreight[withfreight['status']=='EXPIRED']['freight']/1.00 #actual value redacted to avoid compromising anonymity
freight = [pur4, can4, exp4]

# Boxplots of freight price broken down by status
# Set Plot size and title
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart price broken down by status', fontsize=35, fontweight='bold')
# Set line styles
boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
# Boxplot
bp = axs.boxplot(skudata, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Price ($USD)', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)
fig.savefig('cart_price2_box.png')

# Boxplots of price broken down by status, replacing na real price with skuPrice
# Set Plot size and title
fig, axs = plt.subplots(figsize=(16,13))
fig.suptitle('Cart shipping price broken down by status', fontsize=35, fontweight='bold')
# Set line styles
boxprops = dict(linestyle='-', linewidth=3, color='black')
medianprops = dict(linestyle='-', linewidth=2.5, color='red')
# Boxplot
bp = axs.boxplot(freight, showfliers = False, whiskerprops = dict(linestyle='-', linewidth=3),medianprops=medianprops, boxprops=boxprops)
axs.set_xticklabels(['PURCHASED','CANCELED','EXPIRED'])
axs.set_ylabel('Shipping Price ($USD)', fontsize = 35.0)
axs.tick_params(labelsize=35, width=3)
axs.grid(True, axis='y')
for cap in bp['caps']:
    cap.set(linewidth = 3)
fig.savefig('cart_shipping_box.png')
