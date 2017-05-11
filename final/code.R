# yilun chen
# set k & p
setwd("D:/大四下/实验设计/final")
library(tidyverse)
library(glmnet)
library(MASS)
library(ggplot2)
library(randomForest)
require(C50)
require(xgboost)
require(boot)
require(msgl)
require(h2o)
require(RWeka)
require(RSNNS)
kset<-read.csv("k.csv")
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
data1<-data00[[1]]
data2<-data00[[2]]
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

######### part 1 ################

######## outlier ##############
lm3<-lm(response~.,data=data1)
lm4<-lm(response~.,data=data2)
plot(lm3)
plot(lm4)
cooks.distance(lm3)%>%sort(decreasing=TRUE)%>%head()
cooks.distance(lm4)%>%sort(decreasing=TRUE)%>%head()
4/(568-201)
lm3<-lm(response~.,data=data1[-496,])
lm4<-lm(response~.,data=data2[-496,])
plot(lm3)
plot(lm4)
cooks.distance(lm3)%>%sort(decreasing=TRUE)%>%head()
cooks.distance(lm4)%>%sort(decreasing=TRUE)%>%head()
#496 and 310 are outlier
data1[467,1]=0.1
data2[467,1]=0.1
bc<-boxcox(lm3,lambda=seq(-2,2,1/10))
boxcox(lm4,lambda=seq(-2,2,1/10))
bc$x[which.max(bc$y)]#0.38
1/0.38
lm3<-lm(response^0.38~.,data=data1[c(-496,-310),])
lm4<-lm(response^0.38~.,data=data2[c(-496,-310),])
plot(lm3)
plot(lm4)
########## use GMC select 1st subset ##############
gmc<-list(0)
for(i in 2:201)
{
  gmc[[i]]<-GMC(data1[c(-496,-310),c(i,1)])
}
gmc<-unlist(gmc)%>%as.numeric()
xgmc<-gmc[c(TRUE,FALSE)]
ygmc<-gmc[c(FALSE,TRUE)]
hist(ygmc)
summary(ygmc)
subset1<-data1[,c(FALSE,ygmc>0.185)]
dim(subset1)
subset2<-subset1[c(-496,-310),]

######## glm use full data ################

######## cv and lasso #############
data<-kset
lm1<-cv.glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),nfolds = 5)
lm1$nzero
plot(lm1)
(mse1<-(sum(data[,1]-predict(lm1,as.matrix(data[,-1]),s="lambda.min"))^2)/568)
yhat1<-predict(lm1,as.matrix(kset[,-1]),s="lambda.min")
######### elastic ###############
lm2<-list(0)
for(i in 1:101)
{
lm2[[i]]<-glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),standardize = FALSE,alpha = (i-1)/100)
}
lm2<-glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),standardize = FALSE,alpha = 0.5)
lm2$df[32]
lm2<-glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),standardize = FALSE)
lm2<-glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),standardize = FALSE,alpha = 0)
yhat2<-predict(lm2,as.matrix(kset[,-1]))
lm3<-glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),standardize = FALSE,lambda = 3.99)
init<-rbind(lm3$a0,lm3$beta)%>%as.vector()
######### machine learing ##################
### randomforest #######3
X<-kset[,2:61]
dim(X)
sam<-sample(566,floor(566*0.3))
X_train<-kset[-sam,2:61]
X_test<-kset[sam,2:61]
y_train<-kset[-sam,1]
y_test<-kset[sam,1]
m1<-randomForest(y_train~.,data=cbind(X_train,y_train),mtry = 8)
summary(m1)
pre1<-predict(m1,X_test)
mean(abs(pre1-as.matrix(y_test)))#0.3365
p<-varImpPlot(m1)


train_x<-as.matrix(X_train)
train_y<-as.matrix(y_train)
test_x<-as.matrix(X_test)
test_y<-as.matrix(y_test)
dtrain <- xgb.DMatrix(train_x, label = train_y)
dtest <- xgb.DMatrix(test_x, label = test_y) 
watchlist <- list(eval = dtest, train = dtrain)
param <- list(subsample=0.7,eta=0.5,gamma=0.5,max_depth = 4, silent = 1, objective = "reg:linear",eval_metric = "mae")
bst <- xgb.train(booster="gblinear",param, dtrain, nrounds = 100, watchlist)#0.4191



bst1<-xgb.importance(colnames(train_x),bst)
p2<-xgb.plot.importance(bst1,rel_to_first =FALSE)
pred_s <- predict(bst, as.matrix(kset[,-1]))





p2$Feature[1:15]








########## part 2 ############
X0<-cbind(1,kset[,-1])%>%as.matrix
g0 <- function(x){
  x^2
}
g1 <- function(x){
  y <- apply(x,MARGIN = 1,exp)
  return(as.numeric(y))
}
g2 <- function(x){
  x^2.6
}
g3 <- function(x){
  4000/(1+exp(-x))
}
g4 <- function(x){
  4000*(x/(1+abs(x))+1)
}
g5 <- function(x){
  ifelse(x>0,x,0)
}
g6 <- function(x){
  ifelse(x>0,exp(x)-1,x)
}
g7 <- function(x){
  log(1+exp(x))
}
g8 <- function(x){
  (sqrt(x^2+1)-1)/2+x
}
g9 <- function(x){
  x
}
f<-function(beta,x,m,n){
  beta=as.matrix(beta)
  yHat <- g(x%*%beta)
  -var(yHat)/(var(yHat)+var(kset[,1]-yHat))+m*abs(cov(yHat,kset[,1]-yHat))+n*sum(abs(beta))
}
######### choose g ################
###### x^2 ################3
g <- g0;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

s<-seq(-5,1,0.5)
for(i in 1:13)
{
  for(j in 1:13)
  {
    g <- g0;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=10^s[i],n=10^s[j])
    print(c(i,j,GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2]))
  }
}

lm4<-glmnet(as.matrix(kset[,-1]),as.matrix(kset[,1]^2),standardize = FALSE)
yhat4<-predict(lm4,as.matrix(kset[,-1]))
for(i in 1:100)
{
  print(c(i,GMC(matrix(c(yhat4[,i],as.numeric(kset[,1]^2)),ncol=2))[2],GMC(matrix(c(yhat4[,i]^0.5,as.numeric(kset[,1])),ncol=2))[2]))
}
lm3<-glmnet(as.matrix(data[,-1]),as.matrix(data[,1]),standardize = FALSE,lambda = 3.99)
init<-rbind(lm3$a0,lm3$beta)%>%as.vector()

lm5<-glmnet(as.matrix(pset[,-1]),as.matrix(pset[,1]),standardize = FALSE)
yhat5<-predict(lm5,as.matrix(pset[,-1]))
for(i in 1:100)
{
  print(c(i,GMC(matrix(c(yhat5[,i],as.numeric(pset[,1])),ncol=2))[2]))
}


########## 6 X ##############3
######### x^2 ############

name<-c("CYP3A43","CAP1","KCNS3","PYY","HOXC10","EPB41L5")
subset3<-subset2[,names(subset2)%in%name]
subset4<-cbind(response=data1$response[c(-496,-310)],subset3)
X0<-cbind(1,subset4[,-1])%>%as.matrix
g0 <- function(x){
  x^2
}
f<-function(beta,x,m,n){
  beta=as.matrix(beta)
  yHat <- g(x%*%beta)
  -var(yHat)/(var(yHat)+var(subset4[,1]-yHat))+m*abs(cov(yHat,subset4[,1]-yHat))+n*sum(abs(beta))
}
g <- g0;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#1
for(i in 1:20)
{ 
g <- g0;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.86
############ g(x) ###########
g <- g1;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.35,0.36
g <- g2;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.23,1
g <- g3;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.17,0.05
g <- g4;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.15,0.03
g <- g5;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.22,1
g <- g6;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.35,0.36
g <- g7;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.25,0.07
g <- g8;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.23,0.99
g <- g9;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.22,1
for(i in 1:20)
{ 
  g <- g1;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.897
for(i in 1:20)
{ 
  g <- g2;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.88
for(i in 1:20)
{ 
  g <- g3;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.2
for(i in 1:20)
{ 
  g <- g4;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.38
for(i in 1:20)
{ 
  g <- g5;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.05
for(i in 1:20)
{ 
  g <- g6;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.99
for(i in 1:20)
{ 
  g <- g7;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.05
for(i in 1:20)
{ 
  g <- g8;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.07
for(i in 1:20)
{ 
  g <- g9;tmpBeta <- optim(par=rep(0,7),f,method='SANN',x=X0,m=0,n=0)
  print(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.75
}#0.05

######### 15 x #############
name<-p2$Feature[1:15]
subset5<-data1[c(-496,-310),names(data1)%in%name]
subset6<-cbind(response=data1$response[c(-496,-310)],subset5)
X0<-cbind(1,subset6[,-1])%>%as.matrix
g0 <- function(x){
  x^2
}
f<-function(beta,x,m,n){
  beta=as.matrix(beta)
  yHat <- g(x%*%beta)
  -var(yHat)/(var(yHat)+var(subset6[,1]-yHat))+m*abs(cov(yHat,subset6[,1]-yHat))+n*sum(abs(beta))
}
g <- g0;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))
g <- g0;tmpBeta <- optim(par=rep(0,16),f,method='SANN',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))
############ g(x) ###########
g <- g1;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.35,0.36
g <- g2;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.23,1
g <- g3;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.17,0.05
g <- g4;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.15,0.03
g <- g5;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.22,1
g <- g6;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.35,0.36
g <- g7;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.25,0.07
g <- g8;tmpBeta <- optim(par=rep(0,16),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.23,0.99
g <- g9;tmpBeta <- optim(par=rep(0,7),f,method='Nelder-Mead',x=X0,m=0,n=0)
(GMC(matrix(c(g(X0%*%tmpBeta$par),as.numeric(data1[c(-496,-310),1])),ncol=2)))#0.22,1


########### 200 ##############
data2<-data2[c(-496,-310),]
lm6<-glmnet(as.matrix(data2[,-1]),as.matrix(data2[,1]),standardize = FALSE)
yhat6<-predict(lm6,as.matrix(data2[,-1]))
for(i in 1:100)
{
  print(c(i,GMC(matrix(c(yhat6[,i],as.numeric(data2[,1])),ncol=2))[2]))
}

data1<-data1[c(-496,-310),]#####dont do twice

lm7<-glmnet(as.matrix(data1[,-1]),as.matrix(data1[,1]),standardize = FALSE)
yhat7<-predict(lm7,as.matrix(data1[,-1]))
for(i in 1:100)
{
  print(c(i,GMC(matrix(c(yhat7[,i],as.numeric(data1[,1])),ncol=2))[2]))
}
name<-lm7$beta[,65]%>%abs()%>%sort(decreasing=TRUE)%>%head(20)%>%names()
newx<-data1[,names(data1)%in%name]
newdata<-cbind(response=data1$response,newx)
lm8<-lm(response~.,data=newdata)
GMC(cbind(lm8$fitted.values,newdata$response))





init1<-c(lm7$a0[20],lm7$beta[,20])

############ another ################
g <- g1;tmpBeta <- optim(par=rnorm(61,0,10),f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(data[c(-496,-310),1])),ncol=2))[2])

g <- g2;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

g <- g3;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0.1,n=0.1)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

g <- g4;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

g <- g5;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

g <- g6;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

g <- g7;tmpBeta <- optim(par=init,f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

g <- g8;tmpBeta <- optim(par=rnorm(61,0,10),f,method='CG',x=X0,m=0,n=0)
(GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2])

########## inv g ################3


















################################
GMC.matrix.g0 <- matrix(numeric(5*5),ncol=5)
set.seed(602)
for(i in 3:7){
  for(j in 3:7){
    ###choose g0 as the g function
    g <- g1
    tmpBeta <- optim(par=rnorm(61,1,10),f,method='CG',x=X0,m=1*10^(-i),n=1*10^(-j))
    GMC.matrix.g0[i-2,j-2] <- GMC(matrix(c(X0%*%tmpBeta$par,
                                           as.numeric(data[c(-496,-310),1])),ncol=2))[2]
  }
}
colnames(GMC.matrix.g0) <- c("lambda1=e-3","lambda1=e-4","lambda1=e-5","lambda1=e-6","lambda1=e-7")
rownames(GMC.matrix.g0) <- c("lambda2=e-3","lambda2=e-4","lambda2=e-5","lambda2=e-6","lambda2=e-7")
GMC.matrix.g0

optim(par=rnorm(201,1,10),f,method='CG',x=X0,m=1,n=1)
optim(par=rnorm(201,1,10),f,method='BFGS',x=X0,m=1,n=1)

########## time cost ############
set.seed(602)
system.time(optim(par=rnorm(201,1,10),f,method='CG',x=X0,m=1,n=1))#32s
set.seed(602)
system.time(optim(par=rnorm(201,1,10),f,method='BFGS',x=X0,m=1,n=1))#31s
set.seed(602)
system.time(optim(par=rnorm(201,1,10),f,method='Nelder-Mead',x=X0,m=1,n=1))#0.4s
##### NM not require gradient, so its optim might be bad ########

########## part 3 ############
I_y<-(data[,1]>summary(data[,1])[3])%>%as.numeric()
logit1<-glmnet(as.matrix(data[,-1]),I_y,family="binomial")
summary(logit1)


