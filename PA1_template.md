#PA1 template Peer-graded Assessment
##time evolution of steps vs day of year with missing data
## and histogram of total, average and median steps with missing data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(ggplot2)
library(lattice)
library(impute)

home<-"C:/users/samlt/Documents"
setwd(home)

act<-read.csv("activity.csv")

act<-mutate(act,doy=strftime(date,format="%j"))
act$doy<-as.numeric(act$doy)
act<-mutate(act,Day.of.Week=wday(act$date))
act<-mutate(act,wkend=Day.of.Week)
actOrigin<-act
######
we<-(act$Day.of.Week==6)|(act$Day.of.Week==7)
wd<-(act$Day.of.Week!=6)&(act$Day.of.Week!=7)
act$wkend[we]<-1
act$wkend[wd]<-0
######



act$date<-as.Date(as.character(act$date),"%Y-%m-%d")

########

dt<-group_by(act,doy)
sum<-summarise(dt,stp=sum(steps,na.rm=TRUE))
mean<-summarise(dt,stp=mean(steps,na.rm=TRUE))
median<-summarise(dt,stp=median(steps,na.rm=TRUE))

par(mfrow=c(1,3),mar=c(4,4,1,1))
barplot(sum$stp,names.arg=sum$doy,main="total no of steps per day",xlab="day of year (with missing values in steps)",ylab="no of steps")
barplot(mean$stp,names.arg=mean$doy,main="mean no of steps per day",xlab="day of year (with missing values in steps)",ylab="no of steps")
barplot(median$stp,names.arg=median$doy,main="median no of steps per day",xlab="day of year (with missing values in steps)",ylab="no of steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
readline("histogram")
```

```
## [1] ""
```

```r
hist(sum$stp,xlab="total no of steps per day",ylab="frequency",main="With missing values")
hist(mean$stp,xlab="average no of steps per day",ylab="frequency",main="With missing values")
hist(median$stp,xlab="median of steps per day",ylab="frequency",main="With missing values")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)
#Plotting of total, Average, Median steps over 5 mins intervals over 24 hrs for weekends and weekdays


```r
# readline("enter key to proceed plotting Day.of.Week intervals")

act<-mutate(act,week=week(date))

activity<-act
actNA<-activity[is.na(activity$steps),]
actNNA<-activity[is.na(activity$steps)==0,]

actemp<-actNNA

actnna<-mutate(actNNA,index=paste(interval,doy))
actnna<-select(actnna,index)
actnna<-mutate(actnna,ind=0)

actna<-mutate(actNNA,index=paste(interval,doy))
actna<-select(actnna,index)
actna<-mutate(actnna,ind=1)


# create a list of NA from act with search key interval+doy
# stored under actNA with only one column with name index

mNA<-mutate(actNA,index=paste(interval,doy))
actNA<-mNA$index
actNA<-data.frame(actNA)
names(actNA)<-"index"

#weekdays

######
act<-subset(act,act$Day.of.Week!=7)
act<-subset(act,act$Day.of.Week!=6)
# #####

act<-actemp

#weekend
act2<-subset(act,act$Day.of.Week==7)
act3<-subset(act,act$Day.of.Week==6)
act<-rbind(act2,act3)
#####4

#readline("press key when ready to proceed ploting the 3 plots weekends")

par(mfrow=c(1,3),mar=c(4,4,2,2))
t<-act$wkend==1
act<-act[t,]
g<-group_by(act,interval)
swe<-summarise(g,total=sum(steps),mean=mean(steps),median=median(steps))


#readline("press key when ready to proceed ploting the 3 plots weekdays")

par(mfrow=c(1,3),mar=c(4,4,2,2))
act<-actemp
t<-act$wkend==0
act<-act[t,]

actwd<-act
g<-group_by(act,interval)
swd<-summarise(g,total=sum(steps),mean=mean(steps),median=median(steps))

readline("ggplot total steps over 5 mins interval")
```

```
## [1] ""
```

```r
p<-ggplot()+geom_point(data=swe,aes(y=swe$total,x=swe$interval),color=rgb(1,0,0,.5))
p<-p+geom_point(data=swd,aes(y=swd$total,x=swd$interval),color=rgb(0,0,1,.5))
p<-p+labs(x="interval",y="steps (weekend in red/ weekday in blue)")
p<-p+labs(title="Total no of steps in each 5 mins interval")
print(p)
```

![plot of chunk 24hrs.steps.for.weekends.weekdays](figure/24hrs.steps.for.weekends.weekdays-1.png)

```r
readline("ggplot mean steps over 5 mins interval")
```

```
## [1] ""
```

```r
p<-ggplot()+geom_point(data=swe,aes(y=swe$mean,x=swe$interval),color=rgb(1,0,0,.5))
p<-p+geom_point(data=swd,aes(y=swd$mean,x=swd$interval),color=rgb(0,0,1,.5))
p<-p+labs(x="interval",y="steps (weekend in red/ weekday in blue)")
p<-p+labs(title="Mean no of steps in each 5 mins interval")
print(p)
```

![plot of chunk 24hrs.steps.for.weekends.weekdays](figure/24hrs.steps.for.weekends.weekdays-2.png)

```r
readline("ggplot median steps over 5 mins interval")
```

```
## [1] ""
```

```r
p<-ggplot()+geom_point(data=swe,aes(y=swe$median,x=swe$interval),color=rgb(1,0,0,.5))
p<-p+geom_point(data=swd,aes(y=swd$median,x=swd$interval),color=rgb(0,0,1,.5))
p<-p+labs(x="interval",y="steps (weekend in red/ weekday in blue)")
p<-p+labs(title="Median no of steps in each 5 mins interval")
print(p)
```

![plot of chunk 24hrs.steps.for.weekends.weekdays](figure/24hrs.steps.for.weekends.weekdays-3.png)
#Compute max steps in the 5 minutes interval and the specific time in 24 hrs #interval


```r
##### Computing interval with max steps #####

swd2<-cbind(swd,rep(0,length(swd$interval)))
swe2<-cbind(swe,rep(1,length(swe$interval)))
names(swd2)<-c(names(swd),"wkend")
names(swe2)<-c(names(swe),"wkend")
ss<-rbind(swd2,swe2)
m<-max(ss$total)

t<-ss$total==m
print(c("max steps =",m))
```

```
## [1] "max steps =" "7884"
```

```r
if (ss$wkend[t]==1){
  out<-("and it is a weekend")
} else {
  out<-("and it is a weekday")}

print(c("interval(hr)=",ss$interval[t],out))
```

```
## [1] "interval(hr)="       "835"                 "and it is a weekday"
```

```r
###########
```
#Imputing data using k nearest neighbourhood (knn)


```r
readline("imputing activity")
```

```
## [1] ""
```

```r
print(c("no of NAs",sum(is.na(activity$steps))))
```

```
## [1] "no of NAs" "2304"
```

```r
activity<-select(activity,-date)

t<-is.na(activity$steps)==0
activitynna<-activity[t,]

n<-names(activity)

# Imputing

activity<-impute.knn(as.matrix(activity))$data
```

```
## Cluster size 17568 broken into 8784 8784 
## Cluster size 8784 broken into 4445 4339 
## Cluster size 4445 broken into 2250 2195 
## Cluster size 2250 broken into 1050 1200 
## Done cluster 1050 
## Done cluster 1200 
## Done cluster 2250 
## Cluster size 2195 broken into 862 1333 
## Done cluster 862 
## Done cluster 1333 
## Done cluster 2195 
## Done cluster 4445 
## Cluster size 4339 broken into 2195 2144 
## Cluster size 2195 broken into 980 1215 
## Done cluster 980 
## Done cluster 1215 
## Done cluster 2195 
## Cluster size 2144 broken into 2044 100 
## Cluster size 2044 broken into 1174 870 
## Done cluster 1174 
## Done cluster 870 
## Done cluster 2044 
## Done cluster 100 
## Done cluster 2144 
## Done cluster 4339 
## Done cluster 8784 
## Cluster size 8784 broken into 4401 4383 
## Cluster size 4401 broken into 2275 2126 
## Cluster size 2275 broken into 2007 268 
## Cluster size 2007 broken into 844 1163 
## Done cluster 844 
## Done cluster 1163 
## Done cluster 2007 
## Done cluster 268 
## Done cluster 2275 
## Cluster size 2126 broken into 929 1197 
## Done cluster 929 
## Done cluster 1197 
## Done cluster 2126 
## Done cluster 4401 
## Cluster size 4383 broken into 2187 2196 
## Cluster size 2187 broken into 976 1211 
## Done cluster 976 
## Done cluster 1211 
## Done cluster 2187 
## Cluster size 2196 broken into 1220 976 
## Done cluster 1220 
## Done cluster 976 
## Done cluster 2196 
## Done cluster 4383 
## Done cluster 8784
```

```r
activity<-as.data.frame(activity)
names(activity)<-n
actImpute<-activity

set.seed(7890)
actImpute2<-actImpute
actImpute2<-select(actImpute2,-doy,-week)
actImpute2<-actImpute2[sample(8000),]
dis<-dist(actImpute2)
hc<-hclust(dis)
actOrderI<-actImpute2[hc$order,]
```

# Plotting total, mean, median steps over 24 hours interval with imputed data


```r
#plot steps vs interval with imputed data

#readline("press key when ready to proceed ploting the imputed 3 plots weekends")

par(mfrow=c(1,3),mar=c(4,4,2,2))
t<-actImpute$wkend==1
act<-actImpute[t,]

weImputedNA<-mutate(act,index=paste(interval,doy))

act<-merge(weImputedNA,actNA,by="index")
act<-select(act,-index)


g<-group_by(act,interval)
sweI<-summarise(g,total=sum(steps),mean=mean(steps),median=median(steps))

#readline("press key when ready to proceed ploting the Imputed 3 plots weekdays")

par(mfrow=c(1,3),mar=c(4,4,2,2))
t<-actImpute$wkend==0
act<-actImpute[t,]

wdImputedNA<-mutate(act,index=paste(interval,doy))

act<-merge(wdImputedNA,actNA,by="index")
act<-select(act,-index)

g<-group_by(act,interval)

swdI<-summarise(g,total=sum(steps),mean=mean(steps),median=median(steps))

readline("ggplot total steps over 5 mins interval")
```

```
## [1] ""
```

```r
p<-ggplot()+geom_point(data=swe,aes(y=swe$total,x=swe$interval),color=rgb(1,0,0,.5))
p<-p+geom_point(data=sweI,aes(y=sweI$total,x=sweI$interval),color=rgb(0,0,0,.5),pch=16)
p<-p+geom_point(data=swd,aes(y=swd$total,x=swd$interval),color=rgb(0,0,1,.5))
p<-p+geom_point(data=swdI,aes(y=swdI$total,x=swdI$interval),color=rgb(0,0,0,.5),pch=13)
p<-p+labs(x="interval",y="steps (weekend in red/ weekday in blue)")
p<-p+labs(title="Total no of steps in each 5 mins interval (black dots are knn imputed data)")
print(p)
```

![plot of chunk Plotting.Interval](figure/Plotting.Interval-1.png)

```r
readline("ggplot mean steps over 5 mins interval")
```

```
## [1] ""
```

```r
p<-ggplot()+geom_point(data=swe,aes(y=swe$mean,x=swe$interval),color=rgb(1,0,0,.5))
p<-p+geom_point(data=sweI,aes(y=sweI$mean,x=sweI$interval),color=rgb(0,0,0,.5),pch=16)
p<-p+geom_point(data=swd,aes(y=swd$mean,x=swd$interval),color=rgb(0,0,1,.5))
p<-p+geom_point(data=swdI,aes(y=swdI$mean,x=swdI$interval),color=rgb(0,0,0,.5),pch=13)
p<-p+labs(x="interval",y="steps (weekend in red/ weekday in blue)")
p<-p+labs(title="Mean no of steps in each 5 mins interval (black dots are knn imputed data)")
print(p)
```

![plot of chunk Plotting.Interval](figure/Plotting.Interval-2.png)

```r
readline("ggplot median steps over 5 mins interval")
```

```
## [1] ""
```

```r
p<-ggplot()+geom_point(data=swe,aes(y=swe$median,x=swe$interval),color=rgb(1,0,0,.5))
p<-p+geom_point(data=sweI,aes(y=sweI$median,x=sweI$interval),color=rgb(0,0,0,.5),pch=16)
p<-p+geom_point(data=swd,aes(y=swd$median,x=swd$interval),color=rgb(0,0,1,.5))
p<-p+geom_point(data=swdI,aes(y=swdI$median,x=swdI$interval),color=rgb(0,0,0,.5),pch=13)
p<-p+labs(x="interval",y="steps (weekend in red/ weekday in blue)")
p<-p+labs(title="Median no of steps in each 5 mins interval (black dots are knn imputed data) ")
print(p)
```

![plot of chunk Plotting.Interval](figure/Plotting.Interval-3.png)

# Histogram of Imputed Total,Average and Median steps in 5 minutes interval


```r
readline("proceed to calculate histogram of imputed data")
```

```
## [1] ""
```

```r
act<-actImpute
#############^^^^^
histImputed<-mutate(act,index=paste(interval,doy))

actM<-rbind(actna,actnna)

act<-merge(histImputed,actM,by="index")



##############^^^^
dt<-group_by(act,doy)
sumI<-summarise(dt,stp=sum(steps))
meanI<-summarise(dt,stp=mean(steps))
medianI<-summarise(dt,stp=median(steps))

readline("histogram")
```

```
## [1] ""
```

```r
hist(sumI$stp,xlab="total no of steps per day",ylab="frequency")
```

![plot of chunk histogram.imputed](figure/histogram.imputed-1.png)

```r
hist(meanI$stp,xlab="average no of steps per day",ylab="frequency")
```

![plot of chunk histogram.imputed](figure/histogram.imputed-2.png)

```r
hist(medianI$stp,xlab="median of steps per day",ylab="frequency")
```

![plot of chunk histogram.imputed](figure/histogram.imputed-3.png)
