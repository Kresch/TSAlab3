library(TSA)
indata<-read.csv2("fmri.csv",header=TRUE,sep=";")

fmri<-ts(indata$BOLD,start=indata$time[1])
data<-data.frame(indata)

pic_times<-c(20, 60, 100, 140, 180, 220, 260, 300)
X<-c(rep(0:1,each=10,times=8))
X<-c(X[-1],0)
X<-ts(X)
z<-ts(as.matrix(cbind(X,fmri)))
plot(z,main="TS")
acf(X)
pacf(X)
acf(fmri)
pacf(fmri)
ccf(X,fmri)
m1<-arima(X,c(1,0,0))
prewhiten(X,fmri,x.model=m1)

#ccf is max at lag 15 and min at lag 5
#b=3,s=2,r=2... r might be something else
cos_ts<-function(t){
        cos_ts<-c()
        runif(1)->phi
        for (i in 1:t){
                cos_ts<-c(cos_ts,cos(2*pi*(i/20+phi))/2+0.5)  
        }
        return(cos_ts)
}
        
m1<-lm(fmri~X)        
        
        