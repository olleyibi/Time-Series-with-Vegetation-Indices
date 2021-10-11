
# Import Data
toa = read.csv("C:/Users/olley/Downloads/fc_agr_ts_bays_s2toa_2021-07-08.csv",header = T)
suppressMessages(attach(toa))
str(toa)

# Convert time to date data-class
toa$datetime_string_au = as.Date(toa$datetime_string_au,format="%Y-%m-%dT%H:%M:%S")
str(toa)

# Filter out missing values [re74] rows and cloud density greater than 10%
toa = toa[-which(is.na(toa$re74)),]
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

workData=data.frame(vector())

#fld = as.integer(c(284,306,302,296,273,280,253,109))
fld = unique(new_data$field)[15:21]
for (j in fld){
  fd = new_data[which(new_data==j),] # select individual field
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
  workData=rbind(workData,fd[,-c(1,2,20,21,22,23,24,25,26,27,28,29)])
}

fact = c('stage')#,'Sow_method','Variety')
for (i in fact){
  workData[[i]] = as.factor(workData[[i]])
}



workData = cbind(scale(workData[,1:17]),workData[,18])