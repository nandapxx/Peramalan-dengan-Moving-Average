#Input data
data=read.csv(choose.files(),sep = ";")
head(data)
attach(data)

#### N Single Moving Average
n=3
b=nrow(data)-n
## Forecasting
data=cbind(data,forecast=NA,error=NA)
head(data)
fc=rbind(data, c(145, NA, NA,NA))
for (i in 1:(b+1)){
  m=i+(n-1)
  fc$forecast[i+n]=mean(fc$Data[i:m])
  for (i in 1:b){
    fc$error[i+n]=fc$Data[i+n]-fc$forecast[i+n]
  }
}
head(fc)
tail(fc)
##mean error
ME=mean(fc$error,na.rm=TRUE)
ME

#### N Double Moving Average
n=3
##input data
data=read.csv(choose.files(),sep = ";")
data$MA_N=NA
head(data)
b=nrow(data)-n+1
fc=rbind(data, c(145, NA, NA))
tail(fc)
## MA(N)
for (i in 1:b){
  fc$MA_N[i+(n-1)]=mean(fc$Data[i:(i+(n-1))])
}
head(fc)
tail(fc)
## MA(N X N)
fc$MA_NN=NA
head(fc)
for (i in n:b){
  fc$MA_NN[i+(n-1)]=mean(fc$MA_N[i:(i+(n-1))])
}
head(fc)
tail(fc)
## Mencari nilai a
fc$a=NA
head(fc)
for (i in (n+(n-1)):(b+(n-1))){
  fc$a[i]=fc$MA_N[i]+(fc$MA_N[i]-fc$MA_NN[i])
}
head(fc)
tail(fc)
## mencari nilai b
fc$b=NA
head(fc)
for (i in (n+(n-1)):(b+(n-1))){
  fc$b[i]=2*(fc$MA_N[i]-fc$MA_NN[i])/(n-1)
}
head(fc)
tail(fc)
## forecasting untuk m periode kedepan
m=1
fc=cbind(fc,forecast=NA,error=NA)
head(fc)
for (i in (n+n):(b+n)){
  fc$forecast[i]=(fc$a[i-1]+m*fc$b[i-1])
  for (i in (n+n):(b+(n-1)))
    fc$error[i]=fc$Data[i]-fc$forecast[i]
}
head(fc)
tail(fc)
## mean error
ME=mean(fc$error,na.rm = TRUE)
ME
