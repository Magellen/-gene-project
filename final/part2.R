########## part 2 ############
X0<-cbind(1,kset[,-1])%>%as.matrix
g0 <- function(x){
  return(x)
}
g1 <- function(x){
  y <- apply(x,MARGIN = 1,exp)
  return(as.numeric(y))
}
g2 <- function(x){
  x^2.6
}
f<-function(beta,x,m,n){
  beta=as.matrix(beta)
  yHat <- g(x%*%beta)
  -var(yHat)/(var(yHat)+var(kset[,1]-yHat))+m*abs(cov(yHat,kset[,1]))+n*sum(abs(beta))
}
GMC.matrix.g0 <- matrix(numeric(5*5),ncol=5)
set.seed(602)
for(i in 3:7){
  for(j in 3:7){
    ###choose g0 as the g function
    g <- g0
    tmpBeta <- optim(par=rnorm(61,1,10),f,method='CG',x=X0,m=1*10^(-i),n=1*10^(-j))
    
    GMC.matrix.g0[i-2,j-2] <- GMC(matrix(c(X0%*%tmpBeta$par,
                                           as.numeric(data[c(-496,-310),1])),ncol=2))[2]
  }
}
colnames(GMC.matrix.g0) <- c("lambda1=e-3","lambda1=e-4","lambda1=e-5","lambda1=e-6","lambda1=e-7")
rownames(GMC.matrix.g0) <- c("lambda2=e-3","lambda2=e-4","lambda2=e-5","lambda2=e-6","lambda2=e-7")
GMC.matrix.g0

############use 60
tmpBeta <- optim(par=rnorm(61,1,10),f,method='CG',x=X0,m=0,n=0)
GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2]
############use 200
f<-function(beta,x,m,n){
  beta=as.matrix(beta)
  yHat <- g(x%*%beta)
  -var(yHat)/(var(yHat)+var(data1[c(-496,-310),1]-yHat))+m*abs(cov(yHat,data1[c(-496,-310),1]))+n*sum(abs(beta))
}
X0<-cbind(1,data1[c(-496,-310),-1])%>%as.matrix
set.seed(602)
tmpBeta <- optim(par=rnorm(201,1,10),f,method='CG',x=X0,m=0,n=0)
GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(data1[c(-496,-310),1])),ncol=2))[2]

######### 60 X ##################
X0<-cbind(1,kset[,-1])%>%as.matrix
g0 <- function(x){
  return(x)
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
  ln(1+exp(x))
}
g8 <- function(x){
  (sqrt(x^2+1)-1)/2+x
}
f1<-function(beta,x)
{
  beta=as.matrix(beta)
  yHat <- g(x%*%beta)
  -var(yHat)/(var(yHat)+var(kset[,1]-yHat))
}
g<-g2
tmpBeta <- optim(par=rep(0,61),f1,method='Nelder-Mead',x=X0)
tmpBeta <- optim(par=rep(0,61),f1,method='CG',x=X0)
tmpBeta <- optim(par=rep(0,61),f1,method='BFGS',x=X0)
GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2]
g<-g3
tmpBeta <- optim(par=rep(0,61),f1,method='CG',x=X0)
GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2]
g<-g4
tmpBeta <- optim(par=rep(0,61),f1,method='CG',x=X0)
GMC(matrix(c(X0%*%tmpBeta$par,as.numeric(kset[,1])),ncol=2))[2]





GMC(matrix(c(yhat1,as.numeric(kset[,1])),ncol=2))[2]#0.11
for(i in 1:82)
{
  print(c(i,GMC(matrix(c(yhat2[,i],as.numeric(kset[,1])),ncol=2))[2]))
}
#53 X, 0.21 elastic 0.5
#42 X, 0.21 lasso 
GMC(matrix(c(pred_s,as.numeric(kset[,1])),ncol=2))[2]
pre<-read.csv("new.csv",head=FALSE)
GMC(matrix(c(pre$V1,as.numeric(kset[,1])),ncol=2))[2]
GMC(matrix(c(mbs$fitted.values,as.numeric(kset[,1])),ncol=2))[2]

###### g function ##########3
x=seq(1,200,1)
y=x+rnorm(200)
GMC(cbind(x,y))
