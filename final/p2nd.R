rm(list=ls())
setwd("D:/大四下/实验设计/final")
library(tidyverse)
library(randomForest)
library(xgboost)
##########combine data set##############
group_data=function(x)# x is a numb vector
{
  k<-c("a","b","c","d","e","f","g","h",
       "i","k","m","n","o","p","q")#15 set, k is 10, p is 14
  name<-paste("Set_",k[x],".csv",sep = "")
  N=length(name)
  datalist <- list(0)
  read.file <- function(File){
    read.csv(File)
  } 
  datalist <- lapply(name,read.file)#return a list
}

######## k & p ###################

data00<-group_data(c(10,14))
data1<-data00[[2]][c(-496,-310),]
TP<-read.csv("TP53.csv",head=TRUE)
data1<-cbind(response=TP[c(-496,-310),2],data1[,-1])
##### GMC #########
GMC<-function(data,nlocal=25){
  x=data[,1];y=data[,2]
  n=length(x);ln=nlocal
  
  xdata=data[order(data[,1]),];ydata=data[order(data[,2]),]
  E_xy=rep(0,n);E_yx=rep(0,n)
  X=t(matrix(rep(xdata[,1],n),ncol=n)); X=1/(1+abs(X-t(X)))
  Y=t(matrix(rep(ydata[,2],n),ncol=n)); Y=1/(1+abs(Y-t(Y)))
  for(i in 1:n){
    li=max(1,i-ln)
    ui=min(n,i+ln)
    E_yx[i]=sum(X[i,li:ui]*xdata[li:ui,2])/sum(X[i,li:ui]); 
    E_xy[i]=sum(Y[i,li:ui]*ydata[li:ui,1])/sum(Y[i,li:ui])
  }
  GMC=c(var(E_xy)/var(x),var(E_yx)/var(y))
  
  return(GMC)
}

### model #######

m1<-randomForest(response~.,data=data1)
p<-varImpPlot(m1)
p%>%sort(decreasing=TRUE)
names(p)<-dimnames(p)[[1]]
imp<-p%>%sort(decreasing=TRUE)%>%names()
alpha<-numeric(0)
gmc<-numeric(0)
gmclambda<-numeric(0)
gmcalpha<-numeric(0)
for(n in 5:150)
{
  imp1<-imp[1:n]
  subset<-data1[,names(data1)%in%imp1]
  subset1<-cbind(response=data1$response,subset)
  X0<-cbind(1,subset1[,-1])%>%as.matrix
  for (i in 1:15)
  {
    alpha[i]<-seq(0.1,3,0.2)[i]
    f<-function(beta,x,lambda,alpha)
      {
      beta=as.matrix(beta)
      yHat <- (x%*%beta)^alpha
      -var(yHat)/(var(yHat)+var(subset1[,1]-yHat))+lambda*abs(cov(yHat,subset1[,1]-yHat))
      }
    for(j in 1:17)
    {
      s<- seq(0,5,0.3)
      tryCatch({
        tmpBeta <- optim(par=c(lm((response)^(1/alpha[i])~.,data=subset1)$coefficients),f,method='Nelder-Mead',x=X0,lambda=10^-s[i],alpha=alpha[i])
        gmc[j]<-GMC(matrix(c((X0%*%tmpBeta$par)^alpha[i],as.numeric(subset1[,1])),ncol=2))[2]
        print(j)
      }, warning = function(w) {
        gmc[j]<-0
      }, error = function(e) {
        gmc[j]<-0
      })
      
    }
    gmclambda[i]<-gmc%>%na.omit()%>%max
    print(i*1000)
  }
  gmcalpha[n]<-gmclambda%>%na.omit()%>%max
  print(n*1000000)
}
############# set p TP

########## use GMC select 1st subset ##############
gmc<-list(0)
for(i in 2:201)
{
  gmc[[i]]<-GMC(scale(data1[,c(i,1)]))
}
gmc<-unlist(gmc)%>%as.numeric()
xgmc<-gmc[c(TRUE,FALSE)]
ygmc<-gmc[c(FALSE,TRUE)]
names(ygmc)<-names(data1)[-1]
imp2<-ygmc%>%sort(decreasing=TRUE)%>%names
alpha<-numeric(0)
gmc<-numeric(0)
gmclambda<-numeric(0)
gmcalpha<-numeric(0)
for(n in 5:150)
{
  imp1<-imp2[1:n]
  subset<-data1[,names(data1)%in%imp1]
  subset1<-cbind(response=data1$response,subset)
  X0<-cbind(1,subset1[,-1])%>%as.matrix
  for (i in 1:15)
  {
    alpha[i]<-seq(0.1,3,0.2)[i]
    f<-function(beta,x,lambda,alpha)
    {
      beta=as.matrix(beta)
      yHat <- (x%*%beta)^alpha
      -var(yHat)/(var(yHat)+var(subset1[,1]-yHat))+lambda*abs(cov(yHat,subset1[,1]-yHat))
    }
    for(j in 1:17)
    {
      s<- seq(0,5,0.3)
      tryCatch({
        tmpBeta <- optim(par=c(lm((response)^(1/alpha[i])~.,data=subset1)$coefficients),f,method='Nelder-Mead',x=X0,lambda=10^-s[i],alpha=alpha[i])
        gmc[j]<-GMC(matrix(c((X0%*%tmpBeta$par)^alpha[i],as.numeric(subset1[,1])),ncol=2))[2]
        print(j)
      }, warning = function(w) {
        gmc[j]<-0
      }, error = function(e) {
        gmc[j]<-0
      })
      
    }
    gmclambda[i]<-gmc%>%na.omit()%>%max
    print(i*1000)
  }
  gmcalpha[n]<-gmclambda%>%na.omit()%>%max
  print(n*1000000)
}

############xgboost
train_x<-as.matrix(data1[,-1])
train_y<-as.matrix(data1[,1])
#test_x<-as.matrix(X_test)
#test_y<-as.matrix(y_test)
dtrain <- xgb.DMatrix(train_x, label = train_y)
#dtest <- xgb.DMatrix(test_x, label = test_y) 
watchlist <- list(train = dtrain)
param <- list(subsample=0.8,eta=0.45,gamma=0.6,max_depth = 4, silent = 1, objective = "reg:linear",eval_metric = "mae")
bst <- xgb.train(booster="gblinear",param, dtrain, nrounds = 100, watchlist)#0.4191
bst1<-xgb.importance(colnames(train_x),bst)
p2<-xgb.plot.importance(bst1,rel_to_first =FALSE)
imp3<-p2$Feature
alpha<-numeric(0)
gmc<-numeric(0)
gmclambda<-numeric(0)
gmcalpha<-numeric(0)
for(n in 5:150)
{
  imp1<-imp3[1:n]
  subset<-data1[,names(data1)%in%imp1]
  subset1<-cbind(response=data1$response,subset)
  X0<-cbind(1,subset1[,-1])%>%as.matrix
  for (i in 1:15)
  {
    alpha[i]<-seq(0.1,3,0.2)[i]
    f<-function(beta,x,lambda,alpha)
    {
      beta=as.matrix(beta)
      yHat <- (x%*%beta)^alpha
      -var(yHat)/(var(yHat)+var(subset1[,1]-yHat))+lambda*abs(cov(yHat,subset1[,1]-yHat))
    }
    for(j in 1:17)
    {
      s<- seq(0,5,0.3)
      tryCatch({
        tmpBeta <- optim(par=c(lm((response)^(1/alpha[i])~.,data=subset1)$coefficients),f,method='Nelder-Mead',x=X0,lambda=10^-s[i],alpha=alpha[i])
        gmc[j]<-GMC(matrix(c((X0%*%tmpBeta$par)^alpha[i],as.numeric(subset1[,1])),ncol=2))[2]
        print(j)
      }, warning = function(w) {
        gmc[j]<-0
      }, error = function(e) {
        gmc[j]<-0
      })
      
    }
    gmclambda[i]<-gmc%>%na.omit()%>%max
    print(i*1000)
  }
  gmcalpha[n]<-gmclambda%>%na.omit()%>%max
  print(n*1000000)
}

### exp family #######

m1<-randomForest(response~.,data=data1)
p<-varImpPlot(m1)
p%>%sort(decreasing=TRUE)
names(p)<-dimnames(p)[[1]]
imp<-p%>%sort(decreasing=TRUE)%>%names()
alpha<-numeric(0)
gmc<-numeric(0)
gmclambda<-numeric(0)
gmcalpha<-numeric(0)
for(n in 5:150)
{
  imp1<-imp[1:n]
  subset<-data1[,names(data1)%in%imp1]
  subset1<-cbind(response=data1$response,subset)
  X0<-cbind(1,subset1[,-1])%>%as.matrix
  for (i in 1:39)
  {
    alpha[i]<-seq(10,390,10)[i]
    f<-function(beta,x,lambda,alpha)
    {
      beta=as.matrix(beta)
      yHat <- ifelse(x%*%beta>0,x%*%beta+alpha,alpha*(exp(x%*%beta)-1)+alpha)
      -var(yHat)/(var(yHat)+var(subset1[,1]-yHat))+lambda*abs(cov(yHat,subset1[,1]-yHat))
    }
    for(j in 1:17)
    {
      s<- seq(0,5,0.3)
      tryCatch({
        #ifelse((response+0.1)>alpha[i],(response+0.1),log((response+0.1)/alpha[i]))
        tmpBeta <- optim(par=c(lm(ifelse((response+0.1)>alpha[i],(response+0.1),log((response+0.1)/alpha[i]))~.,data=subset1)$coefficients),
                         f,method='Nelder-Mead',x=X0,lambda=10^-s[i],alpha=alpha[i])
        gmc[j]<-GMC(matrix(c(ifelse(X0%*%tmpBeta$par>0,X0%*%tmpBeta$par+alpha[i],alpha[i]*(exp(X0%*%tmpBeta$par)-1)+alpha[i]),as.numeric(subset1[,1])),ncol=2))[2]
        print(j)
      }, warning = function(w) {
        gmc[j]<-0
      }, error = function(e) {
        gmc[j]<-0
      })
      
    }
    gmclambda[i]<-gmc%>%na.omit()%>%max
    print(i*1000)
  }
  gmcalpha[n]<-gmclambda%>%na.omit()%>%max
  print(n*1000000)
}

########### exp ############3
for(n in 5:150)
{
  imp1<-imp[1:n]
  subset<-data1[,names(data1)%in%imp1]
  subset1<-cbind(response=data1$response,subset)
  X0<-cbind(1,subset1[,-1])%>%as.matrix
  for (i in 1)
  {
    
    f<-function(beta,x,lambda)
    {
      beta=as.matrix(beta)
      yHat <- exp(x%*%beta)
      -var(yHat)/(var(yHat)+var(subset1[,1]-yHat))+lambda*abs(cov(yHat,subset1[,1]-yHat))
    }
    for(j in 1:17)
    {
      s<- seq(0,5,0.3)
      tryCatch({
        tmpBeta <- optim(par=c(lm(log(response+0.1)~.,data=subset1)$coefficients),f,method='Nelder-Mead',x=X0,lambda=10^-s[i])
        gmc[j]<-GMC(matrix(c(exp(X0%*%tmpBeta$par),as.numeric(subset1[,1])),ncol=2))[2]
        print(j)
      }, warning = function(w) {
        gmc[j]<-0
      }, error = function(e) {
        gmc[j]<-0
      })
      
    }
    gmclambda[i]<-gmc%>%na.omit()%>%max
    print(i*1000)
  }
  gmcalpha[n]<-gmclambda%>%na.omit()%>%max
  print(n*1000000)
}