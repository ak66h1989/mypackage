

lm<- function(y='r1',model=c('lm','step'),action=c('p','s')){
  
  r.df<-as.data.frame(r)
  r.df<-r.df[,c('年月日',y)]
  r.dt<-data.table::as.data.table(r.df)
  xy.dt<-merge(x.dt,r.dt, by = '年月日',all=T)
  xy.dt<-subset(xy.dt, select =-c(年月日))
  dt<-xy.dt[complete.cases(xy.dt[,y]),]
  fit <- stats::lm(as.formula(paste(y,'~ .')), data=dt)
  if (model=='lm'&&action=='p'){
    par(mfrow=c(2,2))
    plot(fit)
    
  }else if(model=='lm'&&action=='s'){summary(fit)}
    
  else if(model=='step'&&action=='p'){
    dt<-dt[complete.cases(dt),]
    step <- MASS::stepAIC(fit, direction="both") #missing values are not allowed
    step$anova # display results
    fit1<-eval(step$call)
    plot(fit1)
  }
  
  else if(model=='step'&&action=='s'){
    dt<-dt[complete.cases(dt),]
    step <- MASS::stepAIC(fit, direction="both") #missing values are not allowed
    step$anova # display results
    fit1<-eval(step$call)
    summary(fit1)
  }else{summary(fit)}
  
    
 
  }
  





