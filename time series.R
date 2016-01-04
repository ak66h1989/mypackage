myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12) 

# subset the time series (June 2014 to December 2014)
myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12)) 

# plot series
plot(myts)

plot(m[1:3])
m$?~????<-as.Date(m$?~????)
myts <- ts(m['r1'])
plot(myts)
str(m)
m[1]

library(forecast)
fit <- arima(myts[,1])
fit
myts[,1]
Acf(myts)
Pacf(myts)
Acf(fit$residuals^2)
Pacf(fit$residuals^2)
# unit root test
library(tseries)
adf.test(m1[,'r6']) #?t?I?Z?P?Ͷ?

library(urca)
m1<-m[complete.cases(m[,'r6']),]
nrow(m1)
ur.df(m1[,'r6'],type='none',lags=20,selectlags ='AIC') #???t?I?Z?P?Ͷ?

for(i in 2:ncol(m)){m[[i]]<-as.numeric(gsub(",","",m[[i]]))}
str(m)
fit<-lm(r1~mv+pb+ps+pe+dp,data=df)
summary(fit)

library(plm)
str(equind)
equind$?~????<-as.Date(equind$?~????)
equind<-equind[complete.cases(equind[,'???S?v.Ln']),]
equind<-pdata.frame(equind, index = c('?Ҩ??N?X','?~????'), drop.index = FALSE, row.names = TRUE)
purtest(equind$???S?v.Ln,fixedT = F)

library(punitroots)
data("OECDunemp")
df<-as.data.frame(OECDunemp)
df[1]
df<-as.dOECDunemp[,1:ncol(OECDunemp)]
OECDunemp[,-8]
u <- log(OECDunemp[,-8]/(100-OECDunemp[,-8]))
u <- as.data.frame(u)
colnames(u) <- colnames(OECDunemp)[-8]
Choi <- pCADFtest(Y=u, type = "drift", max.lag.y = 5, criterion = "AIC",crosscorr = 0)
pADF <- pCADFtest(Y=u, type = "drift", max.lag.y = 5, criterion = "AIC")
summary(pADF)
library(reshape)
names(equind)
re<-cast(equind, formula = ?~???? ~ ?Ҩ??N?X, 
     margins=FALSE, subset=TRUE, df=FALSE, fill=NULL, add.missing=FALSE,
     value = '???S?v.')
re1<-re[2:ncol(re)]
Choi <- pCADFtest(Y=re1, type = "drift", max.lag.y = 5, criterion = "AIC",crosscorr = 0)

library(nlme)
df<-equindm
names(df)
df<-as.data.frame(dt4)
df1<-df[complete.cases(df[,'r1']),]
fit<-lm(r6 ~mv+pe+ps+pb+dp,data=df)
summary(fit)
plot(fit)
plot(fit$residuals,df$mv)
g <- gls(r1 ~mv+pe+ps+pb+dp,correlation=corAR1(form = ~ 1), data=df1)
g <- gls(r1 ~mv+pe+ps+pb+dp,correlation=corARMA(form = ~ 1,p=1,q=1), data=df1)
summary(g)
intervals(g)

##correlation
res<-as.data.frame(fit$residuals)
names(res)<-'res'
df<-merge(dt3,res,by=0,all=T)
df<-df[complete.cases(df[,'fit$residuals']),]

library(gclus)
dta <- df[,c('fit$residuals','mv','pe','pb','ps','dp')] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5)

library(corrplot)
corrplot.mixed(dta.r,lower = "ellipse", upper = "number")

library(forecast)
a <- arima(fit$residuals)
a
Acf(fit$residuals)
Pacf(fit$residuals)

# unit root test
library(tseries)
adf.test(df1[,'r1']) #?t?I?Z?P?Ͷ?

library(urca)
ur.df(df1[,'r1'],type='none',lags=20,selectlags ='AIC') #???t?I?Z?P?Ͷ?

library(fGarch)
names(df1)
fg <-garchFit(formula = r1~ garch(1, 1),data=df1[,c('r1','mv','pe','pb','ps','dp')], cond.dist="std")
fg <- garchFit(formula = ~ garch(1, 1),data=fit$residuals, cond.dist="std")
coef(fg)
summary(fg)
