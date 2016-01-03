#查詢有中文要用cp950或big5,不管資料用哪種編碼儲存
tree <- function(y='r1'){
r.df<-as.data.frame(r)
r.df<-r.df[,c('年月日',y)]
xy.dt<-merge(x.dt,as.data.table(r.df), by = '年月日')
xy.dt<-subset(xy.dt, select = -c(年月日))
dt<-xy.dt[complete.cases(xy.dt[,y]),]

fit <- rpart(as.formula(paste(y,'~ .')),method="anova", data=dt)
par(mfrow=c(2,1))
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.7)
plotcp(fit) # visualize cross-validation results
}


