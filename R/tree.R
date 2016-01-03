#�d�ߦ�����n��cp950��big5,���޸�ƥέ��ؽs�X�x�s
tree <- function(y='r1'){
r.df<-as.data.frame(r)
r.df<-r.df[,c('�~���',y)]
xy.dt<-merge(x.dt,as.data.table(r.df), by = '�~���')
xy.dt<-subset(xy.dt, select = -c(�~���))
dt<-xy.dt[complete.cases(xy.dt[,y]),]

fit <- rpart(as.formula(paste(y,'~ .')),method="anova", data=dt)
par(mfrow=c(2,1))
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.7)
plotcp(fit) # visualize cross-validation results
}

