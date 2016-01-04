library(RMySQL)
library(quantmod)
library(data.table)
library(TTR)
library(DT)
devtools::use_data(dt3)

  x='5522 遠雄'
  sql<-paste0('SELECT * FROM equ where 證券代碼= ',"'",x,"'")
  sql1<-paste0('SELECT * FROM chip where 證券代碼= ',"'",x,"'")

  ## id
  con1<-dbConnect(RMySQL::MySQL(),username = "root",password = "d03724008",host = "localhost", port = 3300,dbname = "maria")
  dbSendQuery(con1,'SET NAMES big5')
  rs <- dbSendQuery(con1, sql)
  id<-dbFetch(rs,-1) # -1 is correct,1 only fetch 500 records
  id<-as.data.table(id)
  id$年月日<-as.Date(id$年月日)

  ## chip
  rs <- dbSendQuery(con1,sql1)
  chip<-dbFetch(rs,-1) # -1 is correct,1 only fetch 500 records
  chip<-as.data.table(chip)
  chip$年月日<-as.Date(chip$年月日)

  ## ttr
  setorderv(id, c("證券代碼", "年月日"), c(1, 1))
  ttr<-id[,.(
    年月日=年月日,
    P=(最高價.元.+最低價.元.+2*收盤價.元.)/4,
    MA5=SMA(收盤價.元., n=5),
    MA20=SMA(收盤價.元., n=20),
    MA60=SMA(收盤價.元., n=60),
    MA120=SMA(收盤價.元., n=120)
  )]
  #can't use  P in the same []
  #':=' can't be written together in the same []
  ttr[,EMA12:=EMA(P, n=12)]
  ttr[,EMA26:=EMA(P, n=26)]
  ttr[,DIF:=EMA12-EMA26] #與凱基同,但國外把DIF稱為MACD
  ttr[,MACD:=EMA(DIF, n=9)] #與凱基同,但國外好像把這個指標叫做signal
  ttr[,MACD1:=MACD(P, nFast = 12, nSlow = 26, nSig = 9)] #equal to DIF if percent = FALSE
  ttr[,OSC:=DIF-MACD] #與凱基同

  ## r
  id[,R.lead:=shift(報酬率.Ln,n=1L)/100]
  r<-id[,.(
    年月日=年月日,
    R025.=SMA(R.lead,n=5)*30,
    R05.=SMA(R.lead,n=10)*30,
    R1.=SMA(R.lead,n=20)*30,
    R2.=SMA(R.lead,n=40)*30,
    R3.=SMA(R.lead,n=60)*30,
    R6.=SMA(R.lead,n=120)*30
  )]
  r[,r025 := scale(R025., center = TRUE, scale = TRUE)]
  r[,r05 := scale(R05., center = TRUE, scale = TRUE)]
  r[,r1 := scale(R1., center = TRUE, scale = TRUE)]
  r[,r2 := scale(R2., center = TRUE, scale = TRUE)]
  r[,r3 := scale(R3., center = TRUE, scale = TRUE)]
  r[,r6 := scale(R6., center = TRUE, scale = TRUE)]
  #r[,r025 := (R025.-mean(R025., na.rm = TRUE))/mean(R025., na.rm = TRUE)]
  #r[,r05 := (R05.-mean(R05., na.rm = TRUE))/mean(R05., na.rm = TRUE)]
  #r[,r1 := (R1.-mean(R1., na.rm = TRUE))/mean(R1., na.rm = TRUE)]
  #r[,r2 := (R2.-mean(R2., na.rm = TRUE))/mean(R2., na.rm = TRUE)]
  #r[,r3 := (R3.-mean(R3., na.rm = TRUE))/mean(R3., na.rm = TRUE)]
  #r[,r6 := (R6.-mean(R6., na.rm = TRUE))/mean(R6., na.rm = TRUE)]
  r$年月日<-as.Date(r$年月日)

  ## index
  TWII<-getSymbols("^TWII",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  SSE<-getSymbols("000001.SS",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  HSI<-getSymbols("^HSI",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  STI<-getSymbols("^STI",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  N225<-getSymbols("^N225",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  AXJO<-getSymbols("^AXJO",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  GSPC<-getSymbols("^GSPC",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  IXIC<-getSymbols("^IXIC",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  GDAXI<-getSymbols("^GDAXI",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  FTSE<-getSymbols("^FTSE",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  STOXX50E<-getSymbols("^STOXX50E",src="yahoo",auto.assign = FALSE,from = as.Date("1980-01-04"))
  c<-cbind(TWII[,6],SSE[,6],HSI[,6],STI[,6],N225[,6],AXJO[,6],GSPC[,6],IXIC[,6],GDAXI[,6],FTSE[,6],STOXX50E[,6])
  for (i in 1:ncol(c)){c[,i]<-(c[,i]-mean(c[,i],na.rm = T))/mean(c[,i],na.rm = T)}
  index<-as.data.table(c)
  names(index)[1]<-'年月日'

  ## idsd
  idsd<-id[,.(
    年月日=年月日,
    E.=市值.百萬元./本益比.TEJ,
    B.=市值.百萬元./股價淨值比.TEJ,
    S.=市值.百萬元./股價營收比.TEJ,
    D.=現金股利率*市值.百萬元.,
    mv = (市值.百萬元.-mean(市值.百萬元., na.rm = TRUE))/mean(市值.百萬元., na.rm = TRUE),
    pe = (本益比.TEJ-mean(本益比.TEJ, na.rm = TRUE))/mean(本益比.TEJ, na.rm = TRUE),
    pb = (股價淨值比.TEJ-mean(股價淨值比.TEJ, na.rm = TRUE))/mean(股價淨值比.TEJ, na.rm = TRUE),
    ps = (股價營收比.TEJ-mean(股價營收比.TEJ, na.rm = TRUE))/mean(股價營收比.TEJ, na.rm = TRUE),
    dp = (現金股利率-mean(現金股利率, na.rm = TRUE))/mean(現金股利率, na.rm = TRUE)

  )]
  idsd[,e := (E.-mean(E., na.rm = TRUE))/mean(E., na.rm = TRUE)]
  idsd[,b := (B.-mean(B., na.rm = TRUE))/mean(B., na.rm = TRUE)]
  idsd[,s := (S.-mean(S., na.rm = TRUE))/mean(S., na.rm = TRUE)]
  idsd[,d := (D.-mean(D., na.rm = TRUE))/mean(D., na.rm = TRUE)]

  idsd[,de6 := (e-shift(e,n=120L,"lead"))/shift(e,n=120L,"lead")] #bug 瞻瞿繙?????ead穢?????ag糧瞿瞻@翹??
  idsd[,ds6 := (s-shift(s,n=120L,"lead"))/shift(s,n=120L,"lead")]
  idsd[,db6 := (b-shift(b,n=120L, "lead"))/shift(b,n=120L,"lead")]
  idsd[,dd6 := (d-shift(d,n=120L,"lead"))/shift(d,n=120L,"lead")]
  idsd[,de12 := (e-shift(e,n=240L, "lead"))/shift(e,n=240L,"lead")]
  idsd[,ds12 := (s-shift(s,n=240L, "lead"))/shift(s,n=240L,"lead")]
  idsd[,db12 := (b-shift(b,n=240L, "lead"))/shift(b,n=240L,"lead")]
  idsd[,dd12 := (d-shift(d,n=240L,"lead"))/shift(d,n=240L,"lead")]

  dt<-merge(subset(id, select = -c(證券代碼)),r, by = '年月日')
  dt<-merge(dt,subset(chip, select = -c(證券代碼)), by = '年月日')
  dt<-merge(dt,ttr, by = '年月日')
  dt<-merge(dt,index, by = '年月日')
  dt<-merge(dt,idsd, by = '年月日')

  dt1<-subset(dt, select = -c(年月日,報酬率.,漲跌停,注意股票.A.,處置股票.D.,全額交割.Y.,市場別,R.lead,R025.,R05.,R1.,R2.,R3.,R6.,r025,r05,r2,r3,r6))
  dt1<-dt1[complete.cases(dt1[,'r1']),]
  # remove collinearity variables
  dt2<-dt[, c('年月日','報酬率.','漲跌停','注意股票.A.','處置股票.D.','全額交割.Y.','市場別','R.lead','R025.','R05.','R1.','R2.','R3.','R6.','r025','r05','r2','r3','r6',
              'DIF','mv','e','b','s','d','P','OSC','未調整收盤價(元)','三大法人賣超(張)','外資賣超(張)','投信賣超(張)','自營賣超(張)') := NULL]
  dt2<-dt2[complete.cases(dt2[,'r1']),]
  dt3<-dt2[complete.cases(dt2),]

<<<<<<< HEAD
=======

>>>>>>> 1e062bf5d462e0c110ff1c780cd8c81a562ddee4
