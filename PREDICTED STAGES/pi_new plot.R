anth=emergence=ff=har=pi=pw=ms=max_dv=min_dv=space=max_cig=vector()
fld = unique(new_data$field) # get field ID
k=0
n = length(fld)


maxdv=mindv=vector()
m=0
pdf("cig_derivative3.pdf", onefile = TRUE)
for(i in seq(ceiling(n/4))) {
  #For the last page
  if(n > 4) k <- 4 else k <- n
  n <- n - 4
  #Create a list to store the plots
  plot_list <- vector('list', k)
  for(j in 1:k) {
    m=m+1
    
    fd = new_data[which(new_data==fld[m]),] # select individual field
    fd = fd[order(fd$date),] # order the rows by the date-time column
    minyr=format(min(fd$date),format="%Y") # get start year
    start_date = as.Date(paste(minyr,"08-08",sep="-")) # set start date to August 1st
    fd = fd[which(fd$date>start_date),] # Filter out date after start date
    
    # get dates for stages
    anth[m]= as.integer(difftime(fd$Anth[1],start_date,units = 'day'))
    har[m]=as.integer(difftime(fd$Harvest[1],start_date,units = 'day'))
    ms[m]=as.integer(difftime(fd$MS[1],start_date,units = 'day'))
    pi[m]=as.integer(difftime(fd$PI[1],start_date,units = 'day'))
    pw[m]=as.integer(difftime(fd$PW[1],start_date,units = 'day'))
    ff[m]=as.integer(difftime(fd$FF[1],start_date,units = 'day'))
    emergence[m]=as.integer(difftime(fd$Emergence[1],start_date,units = 'day'))
    
    
    
    # Get time-series distribution in days
    fd$day_dif = as.integer(difftime(fd$date,
                                     start_date,units = 'day'))
    
    
    # Notable dates for each field
    vert = data.frame(t(fd[1:1,20:26]), stringsAsFactors = FALSE)
    vert = na.omit(vert)
    vert$diff = as.integer(difftime(as.Date(vert[,1],format="%Y-%m-%d"),
                                    start_date,units = 'day'))
    
    
    # # Trim repeating dates
    # fn = as.data.frame(tapply(fd$cig,fd$day_dif,mean))
    # fn <- cbind(day_diff = rownames(fn),cig = fn)
    # rownames(fn) <- 1:nrow(fn)
    # colnames(fn) <- list("day_diff","cig")
    # fn$day_diff = as.numeric(fn$day_diff)
    
    
    
    
    # GAM smoothing model
    mod = gam(cig~s(day_dif,bs='cr'),data=fd) # using cubic regression spline
    fd$pred=predict.gam(mod,fd)
    fd$der = first_diff(fd$day_dif,fd$pred)
    #fd$der = predict(gam(der~s(day_dif,bs='cr'),data=fd))
    #fd$der = predict(loess(der~day_dif,data=fd))
    
    maxdv[m]=max(fd$der)
    mindv[m]=min(fd$der)
    
    # Get derivative value
    max_dv[m]=as.data.frame(fd[fd[,which(fd$der == max(fd$der))],29])[1,1]
    max_cig[m]=as.data.frame(fd[fd[,which(fd$pred == max(fd$pred))],29])[1,1]
    min_dv[m]=as.data.frame(fd[fd[,which(fd$der == min(fd[which(fd$date>fd$PW),]$der))],29])[1,1]
    space[m]=fld[m]
    
    # Plot
    scaleFactor = max(fd$pred)/max(fd$der)
    plot_list[[j]] <- ggplot(fd, aes(day_dif))+
      ggtitle(paste("Field",fld[m],fd$Sow_method[1],fd$Variety[1]))+#  Sow_method[1],fd$Variety[1])) +
      geom_hline(yintercept = 0,lwd=0.5)+
      geom_line(aes(y=pred),col='red') + # GAM cig
      geom_line(aes(y=der*scaleFactor),col='blue')+ # first derivative
      geom_vline(xintercept = vert['PI','diff'],col='red') +
      scale_y_continuous(name="cig", sec.axis=sec_axis(~./scaleFactor, name="Deriv(cig)"))+
      theme(axis.title.y.left=element_text(color="red"),
            axis.title.y.right = element_text(color="blue"))+
      annotate(x = vert['PI','diff'], y = 0.8, label = 'PI',geom='text',
               angle=90,col='red',vjust=-0.4, hjust=1)  
  }
  #Print multiple plots together
  print(do.call(gridExtra::grid.arrange, plot_list))
}
dev.off()


stages = c('ff','pw','emergence','pi','ms','anth','har')
date_stage = data.frame(cbind(space,ff,pw,emergence,pi,ms,anth,har))
library(caret)



try = data.frame(cbind(field=date_stage$space,
                       predict=max_dv,
                       predict1=max_cig,
                       predict2=min_dv,
                       actual=date_stage$pi))

row.names(try)=try$field
try=na.omit(try[-1])
# which(row.names(try)==57) #20
# which(row.names(try)==214) #12
#try=try[-20,]
RMSE(try$predict,try$actual)


ggplot(try,aes(x=actual,y=predict))+
  geom_abline(intercept=0, slope=1)+
  geom_point()+xlim(min(try$predict),max(try$predict)+10)+
  ylim(min(try$predict),max(try$predict)+10)+
  # geom_text(label = row.names(try),,nudge_x = 1, nudge_y =2.5)+
  xlab('Actual')+ylab('Predicted')+
  annotate(geom='text',x=min(try$actual)-5,y=max(try$predict)-5,
           label=paste("RMSE:",round(RMSE(try$predict,try$actual),3)),col='blue')+
  ggtitle("RMSE value for PI using max deriv cig")



max_derv=round(RMSE(try$predict,try$actual),3)
max_vi=round(RMSE(try$predict1,try$actual),3)
min_derv=round(RMSE(try$predict2,try$actual),3)


data.frame(max_vi,max_derv,min_derv)

max(maxdv)
min(mindv)