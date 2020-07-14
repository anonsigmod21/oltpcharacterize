library(forecast)
library(tidyverse)

# pre-requisite: 
# this script assumes that dataPreprocessing.R has been called, which reads the data
# CSV files, joins them by common columns, and applies some data cleaning/preprocessing

# week_list is a list that stores values of the outcome variable by week; there are 15 weeks in total
# aug_full is the complete dataset of August; for trainOnce, select which outcome variable to use from aug_full

#==============cumulativeTrain=================

end_week<-15
arima_mre=matrix(0,end_week,1)

for(k in 2:end_week){
  df_train<-c(week_list[1:k-1], recursive=TRUE)
  df_test<-week_list[[k]]
  p<-11; q<-8; f<-4; # learned from sensitivity analysis & auto.arima()
  order = c(p,0,q)
  
  ar<-arima(df_train, order = order)
  model<-ar
  start=p+1
  preds = matrix(df_test)
  
  for(i in seq(start, length(df_test), f)){ 
    subInters = df_test[1:(i-1)]; 
    new_arima <- Arima(subInters, model=ar)
    nextpoint = predict(new_arima,newdata=subInters, n.ahead=f); 
    j=i;
    for(h in seq(1,f)){
      if(j <= length(df_test) ){   
        preds[j,1]=nextpoint$pred[h];  
      } 
      j=j+1;
    } 
  }
  ac=df_test[start:length(df_test)]; 
  pr=preds[start:length(df_test),1];
  mre = mean(abs( (ac-pr)/ac ), na.rm=T); 
  arima_mre[k,1]<-mre;
}

prev_recur<-arima_mre


#==============sliding2WeeksTrain=============

end_week<-15
arima_mre=matrix(0,end_week,1)

for(k in 3:end_week){
  df_train<-c(week_list[[k-2]],week_list[[k-1]])
  df_test<-week_list[[k]]
  p<-11
  q<-8
  order = c(p,0,q)
  f<-4
  
  ar<-arima(df_train, order = order)
  model<-ar
  start=p+1
  preds = matrix(df_test)
  
  for(i in seq(start, length(df_test), f)){ 
    subInters = df_test[1:(i-1)]; 
    new_arima <- Arima(subInters, model=ar)
    nextpoint = predict(new_arima,newdata=subInters, n.ahead=f); 
    j=i;
    for(h in seq(1,f)){
      if(j <= length(df_test) ){   
        preds[j,1]=nextpoint$pred[h];  
      } 
      j=j+1;
    } 
  }
  ac=df_test[start:length(df_test)]; 
  pr=preds[start:length(df_test),1];
  mre = mean(abs( (ac-pr)/ac ), na.rm=T); 
  arima_mre[k,1]<-mre;
}

prev_2week<-arima_mre


#==============sliding4WeeksTrain==============
end_week<-15
arima_mre=matrix(0,end_week,1)

for(k in 5:end_week){
  df_train<-c(week_list[[k-4]],week_list[[k-3]],week_list[[k-2]],week_list[[k-1]])
  df_test<-week_list[[k]]
  p<-11
  q<-8
  order = c(p,0,q)
  f<-4
  
  ar<-arima(df_train, order = order)
  model<-ar
  start=p+1
  preds = matrix(df_test)
  
  for(i in seq(start, length(df_test), f)){ 
    subInters = df_test[1:(i-1)]; 
    new_arima <- Arima(subInters, model=ar)
    nextpoint = predict(new_arima,newdata=subInters, n.ahead=f); 
    j=i;
    for(h in seq(1,f)){
      if(j <= length(df_test) ){   
        preds[j,1]=nextpoint$pred[h];  
      } 
      j=j+1;
    } 
  }
  ac=df_test[start:length(df_test)]; 
  pr=preds[start:length(df_test),1];
  mre = mean(abs( (ac-pr)/ac ), na.rm=T); 
  arima_mre[k,1]<-mre;
}

prev_4week<-arima_mre


#==============trainOnce=======================

end_week<-15
arima_mre=matrix(0,15,1)

p=11
q=8
order = c(p,0,q)
df_train<-aug_full[,"n_riak"]
ar<-arima(df_train, order = order)

model<-ar
start=p+1

for(k in 4:end_week){  
  df_test<-week_list[[k]]
  preds = matrix(df_test)
  f<-4
  for(i in seq(start, length(df_test), f)){ 
    subInters = df_test[1:(i-1)]; 
    new_arima <- Arima(subInters, model=ar)
    nextpoint = predict(new_arima,newdata=subInters, n.ahead=f); 
    j=i;
    for(h in seq(1,f)){
      if(j <= length(df_test) ){   
        preds[j,1]=nextpoint$pred[h];  
      } 
      j=j+1;
    } 
  }
  ac=df_test[start:length(df_test)]; 
  pr=preds[start:length(df_test),1];
  mre = mean(abs( (ac-pr)/ac ),na.rm=T); 
  arima_mre[k,1]=mre;
}

aug_full_mre<-arima_mre
