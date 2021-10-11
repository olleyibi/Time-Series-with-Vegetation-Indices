# Prepare environment and libraries
rm(list=ls())
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(signal))
suppressMessages(library(mgcv))
suppressMessages(library(data.table))

# Import Data
toa2 = read.csv("C:/Users/olley/Downloads/fc_agr_ts_bays_s2sr_2021-07-08.csv",header = T)
suppressMessages(attach(toa2))
str(toa2)

# Convert time to date data-class
toa2$datetime_string_au = as.Date(toa2$datetime_string_au,format="%Y-%m-%dT%H:%M:%S")
str(toa2)

# Filter out missing values [re74] rows and cloud density greater than 10%
toa2 = toa2[-which(is.na(toa2$re74)),]
toa2 = toa2[which(toa2$CLOUDY_PIXEL_PERCENTAGE<10),]


# Extract Data set
new_data2 = as.data.frame(cbind(field=toa2$ref,
                                date=toa2$datetime_string_au,toa2[,18:34]))
new_data2 = setDT(new_data2)[, lapply(.SD, mean), by = .(field,date)]

for (i in 1:length(row.names(new_data2))){
  fld=new_data2[i,field]
  for (j in colnames(toa2[,7:15])){
    new_data2[[j]][i]=toa2[which(toa2$ref == fld)[1],j]
  }
}

# Convert notable stages to date data-class
stage = c('Anth','Emergence','FF','Harvest','PI','PW','MS')
for (i in stage){
  new_data2[[i]] = as.Date(new_data2[[i]],format="%Y-%m-%d")
}
i=0
workData2=data.frame(vector())

fld = unique(new_data2$field)
for (j in fld){
  fd = new_data2[which(new_data2==j),] # select individual field
  fd = fd[order(fd$date),] # order the rows by the date-time column
  fd = fd[which(fd$date>=fd$FF),]
  fd$day_dif = as.integer(difftime(fd$date,fd$FF,units = 'day'))
  
  day=function(x){
    return(as.integer(difftime(x,fd$FF,units = 'day')))
  }
  class(fd$day_dif)
  n=0
  for (i in fd$day_dif){
    n=n+1
    if (i>=day(fd$FF)[1] & i<day(fd$PI)[1]){
      fd$stage[n]='FF/PW/Emergence'
    } else if (i>=day(fd$PI)[1] & i<day(fd$Anth)[1]){
      fd$stage[n]='PI/MS'
    } else {
      fd$stage[n]= 'Anth/Harvest'
    }
  }
  workData2=rbind(workData2,fd[,-c(1,2,20,21,22,23,24,25,26,27,28,29)])
}

fact = c('stage')#,'Sow_method','Variety')
for (i in fact){
  workData2[[i]] = as.factor(workData2[[i]])
}


workData2 = cbind(scale(workData2[,1:17]),workData2[,18])

library(randomForest)
rf_classifier = randomForest(stage ~ ., data=workData2,
                             ntree=500, mtry=2, importance=TRUE)



library(caret)
y_pred = predict(classifier, newdata = workData)
g=cbind(workData[,18], y_pred)
confusionMatrix(g$stage,g$y_pred)