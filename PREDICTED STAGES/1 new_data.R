# Prepare environment and libraries
rm(list=ls())
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(signal))
suppressMessages(library(mgcv))
suppressMessages(library(data.table))

# Import Data
toa = read.csv("C:/Users/olley/Downloads/fc_agr_ts_bays_s2toa_2021-07-08.csv",header = T)
suppressMessages(attach(toa))
str(toa)

# Convert time to date data-class
toa$datetime_string_au = as.Date(toa$datetime_string_au,format="%Y-%m-%dT%H:%M:%S")
str(toa)

# Filter out missing values [ndvi] rows and cloud density greater than 10%
toa = toa[-which(is.na(toa$ndvi)),]
toa = toa[which(toa$CLOUDY_PIXEL_PERCENTAGE<10),]


# Extract Data set
new_data = as.data.frame(cbind(field=toa$ref,
                               date=toa$datetime_string_au,toa[,18:34]))
new_data = setDT(new_data)[, lapply(.SD, mean), by = .(field,date)]

for (i in 1:length(row.names(new_data))){
  fld=new_data[i,field]
  for (j in colnames(toa[,7:15])){
    new_data[[j]][i]=toa[which(toa$ref == fld)[1],j]
  }
}

# Convert notable stages to date data-class
stage = c('Anth','Emergence','FF','Harvest','PI','PW','MS')
for (i in stage){
  new_data[[i]] = as.Date(new_data[[i]],format="%Y-%m-%d")
}

# First Derivative
first_diff = function(x,y){
  vec=vector(length=length(x))
  m=1
  for (i in 2:length(x)){
    vec[m]=(y[i]-y[i-1])/(x[i]-x[i-1])
    m=m+1
  }
  return(vec)
}
