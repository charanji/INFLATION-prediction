library(readxl)
inflation.data<-read_excel('Inflation.xlsx')
inflation.data2<-inflation.data[,2]
library(tseries)
inflation.data1<-ts(inflation.data2,frequency = 1,start = c(1990,1))

plot(inflation.data1, main='Inflation')

adf.test(inflation.data1,alternative = 'stationary',k=0)
acf(inflation.data1)
pacf(inflation.data1)
Box.test(inflation.data1,lag=log(length(inflation.data1)))


library(forecast)
autoArimaModel<-auto.arima(inflation.data1,d=0)
autoArimaModel

autoArimaModel1<-auto.arima(inflation.data1,d=1)
autoArimaModel1

autoArimaModel2<-auto.arima(inflation.data1,d=2)
autoArimaModel2


#myModel<-arima(x=b,order = c(0,0,0),seasonal = list(order=c(4,0,3),period=4))
#myModel

autoPred = forecast(autoArimaModel, h=3)
plot(autoPred,ylab = 'Inflation in percentage')

p=1
ma.process=inflation.data1-mean(inflation.data1)
acf(ma.process)
pacf(ma.process)
r=NULL
r[1:1]=acf(inflation.data1,plot = F)$acf[2:(p+1)]
cat('r=',r,'\n')
R=matrix(1,p,p)
for (i in 1:p) {
  for (j in 1:p) {
    if(i!=j){
      R[i,j]=r[abs(i-j)]
    }
    
  }
  
}
R
b=NULL
b=matrix(r,p,1)
b
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat
c0=acf(inflation.data1,type = 'covariance',plot = F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat
phi0.hat=mean(inflation.data1)*(1-sum(phi.hat))
phi0.hat

