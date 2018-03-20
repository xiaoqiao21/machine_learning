bupa0 <- read.csv("bupa.data",head=F)


colnames(bupa0) <- c("mcv","alkphos","sgpt","sgot","gammagt","drinks","selector")
bupa <- bupa0
bupa[,7][bupa[,7]==2] <- -1

###The first question

dect <- function(x)
{
  x0 <- unique(x)
  h <- matrix(NA,length(x0),2)
  for(j in 1:length(x0))
  {
    h1 <- h2 <- rep(1,length(x))
    h1[x<x0[j]] <- -1
    h2[x>=x0[j]] <- -1
    h[j,1] <- -sum((h1*training.select*D0)[h1*training.select==-1])
    h[j,2] <- -sum((h2*training.select*D0)[h2*training.select==-1])
  }
  idd <- which(h==min(h),arr.ind=T)
  c0 <- c(x0[idd[1,1]],1,-1,min(h))
  if(idd[1,2]==2)
  {
    c0[2:3] <- c(-1,1) 
  }
  return(c0)
}

trainingerror <- matrix(NA,100,50)
testingerror <- matrix(NA,100,50)
for(k in 1:50)
{
  test <- sort(sample(1:345,35))
  training.feature <- bupa[-test,-7]
  training.select <- bupa[-test,7]
  testing.feature <- bupa[test,-7]
  testing.select <- bupa[test,7]
  error <- rep(0,100)
  error.train <- rep(0,100)
  error.test <- rep(0,100)
  alpha <- c()
  z <- c()
  D <- matrix(NA,310,100)
  ht <- matrix(NA,310,100)
  ht0 <- matrix(NA,35,100)
  Ht.train <- matrix(NA,310,100)
  Ht.test <- matrix(NA,35,100)
  for(i in 1:100)
  {
    if(i==1)
    {
      D[,1] <- rep(1/310,310)
    }
    D0 <- D[,i]
    dectree <- apply(training.feature,2,dect)
    error0 <- as.vector(dectree[4,])
    error[i] <- min(error0)
    decision.tree <- c(which.min(error0)[1],dectree[-4,which.min(error0)[1]])
    feature.train <- as.vector(training.feature[,decision.tree[1]])
    feature.test <-as.vector(testing.feature[,decision.tree[1]])
    alpha[i] <- .5*log((1-error[i])/error[i])
    h0 <- rep(1,310)
    h00 <- rep(1,35)
    if(decision.tree[3]==1)
    {
      h0[feature.train<decision.tree[2]] <- -1
      h00[feature.test<decision.tree[2]] <- -1
    }else
    {
      h0[feature.train>=decision.tree[2]] <- -1
      h00[feature.test>=decision.tree[2]] <- -1
    }
    ht[,i] <- h0
    ht0[,i] <- h00
    z[i] <- sum(D*exp(-alpha[i]*training.select*h0))
    if(i<100)
    {
      D[,i+1] <- D[,i]*exp(-alpha[i]*training.select*h0)
    }
    if(i==1)
    {
      htrot <- sign(ht[,1]*alpha[1])
      hteot <- sign(ht0[,1]*alpha[1])
    }else
    {
      htrot <- sign(rowSums(ht[,1:i]%*%alpha[1:i]))
      hteot <- sign(rowSums(ht0[,1:i]%*%alpha[1:i]))
    }
    htrot[htrot==0] <- sample(c(-1,1),1)
    Ht.train[,i] <- htrot    
    hteot[hteot==0] <- sample(c(-1,1),1)
    Ht.test[,i] <- hteot
    error.train[i] <- length(which(htrot!=training.select))/length(htrot)
    error.test[i] <- length(which(hteot!=testing.select))/length(hteot)
  }
  trainingerror[,k] <- error.train
  testingerror[,k] <- error.test
}
averagetrainingerror <- rowMeans(trainingerror)
averagetestingerror <- rowMeans(testingerror)
plot(1:100,averagetrainingerror,xlab='iterations',ylab='error',ylim=c(0,1),main="Average Error",type='l',)
lines(1:100,averagetestingerror,lty=2)


###ALL data


### The Second question


feature <- bupa[,-7]
selector <- bupa[,7]
error <- rep(0,10)
alpha <- c()
z <- c()
D <- matrix(NA,345,10)
ht <- matrix(NA,345,10)
component <- c()
threshold <- c()
classlabel <- c()

dect2 <- function(x)
{
  x0 <- unique(x)
  h <- matrix(NA,length(x0),2)
  for(j in 1:length(x0))
  {
    h1 <- h2 <- rep(1,length(x))
    h1[x<x0[j]] <- -1
    h2[x>=x0[j]] <- -1
    h[j,1] <- -sum((h1*selector*D0)[h1*selector==-1])
    h[j,2] <- -sum((h2*selector*D0)[h2*selector==-1])
  }
  idd <- which(h==min(h),arr.ind=T)
  c0 <- c(x0[idd[1,1]],1,-1,min(h))
  if(idd[1,2]==2)
  {
    c0[2:3] <- c(-1,1) 
  }
  return(c0)
}


for(i in 1:10)
{
  if(i==1)
  {
    D[,1] <- rep(1/345,345)
  }
  D0 <- D[,i]
  dectree <- apply(feature,2,dect2)
  error0 <- as.vector(dectree[4,])
  error[i] <- min(error0)
  decision.tree <- c(which.min(error0)[1],dectree[-4,which.min(error0)[1]])
  component[i] <- decision.tree[1]
  threshold[i] <- decision.tree[2]
  classlabel[i] <- decision.tree[3]
  feature.train <- as.vector(feature[,decision.tree[1]])
  alpha[i] <- .5*log((1-error[i])/error[i])
  h0 <- rep(1,345)
  if(decision.tree[3]==1)
  {
    h0[feature.train<decision.tree[2]] <- -1
  }else
  {
    h0[feature.train>=decision.tree[2]] <- -1
  }
  ht[,i] <- h0
  z[i] <- sum(D*exp(-alpha[i]*selector*h0))
  if(i<10)
  {
    D[,i+1] <- D[,i]*exp(-alpha[i]*selector*h0)
  } 
}

###Display of Question 2
question2 <- cbind(1:10,component,threshold,classlabel)
colnames(question2) <- c('iterations','component j','threshold c','class label C1')
question2


###Question 3


alpha <- c()
z <- c()
D <- matrix(NA,345,100)
ht <- matrix(NA,345,100)
component <- c()
threshold <- c()
classlabel <- c()
ft <- matrix(NA,345,100)
for(i in 1:100)
{
  if(i==1)
  {
    D[,1] <- rep(1/345,345)
  }
  D0 <- D[,i]
  dectree <- apply(feature,2,dect2)
  error0 <- as.vector(dectree[4,])
  error[i] <- min(error0)
  decision.tree <- c(which.min(error0)[1],dectree[-4,which.min(error0)[1]])
  component[i] <- decision.tree[1]
  threshold[i] <- decision.tree[2]
  classlabel[i] <- decision.tree[3]
  feature.train <- as.vector(feature[,decision.tree[1]])
  alpha[i] <- .5*log((1-error[i])/error[i])
  h0 <- rep(1,345)
  if(decision.tree[3]==1)
  {
    h0[feature.train<decision.tree[2]] <- -1
  }else
  {
    h0[feature.train>=decision.tree[2]] <- -1
  }
  ht[,i] <- h0
  if(i==1)
  {
    ft[,i] <- ht[,i]
  }else
  {
    ft[,i] <- ht[,1:i]%*%alpha[1:i]/sum(alpha)
  }  
  z[i] <- sum(D*exp(-alpha[i]*selector*h0))
  if(i<100)
  {
    D[,i+1] <- D[,i]*exp(-alpha[i]*selector*h0)
  } 
}
fT <- ft[,c(10,50,100)]
margin <- matrix(NA,345,3)
for(i in 1:3)
{
  margin[,i] <- fT[,i]*selector
}
ecdf10 <- knots(ecdf(margin[,1]))
ecdf50 <- knots(ecdf(margin[,2]))
ecdf100 <- knots(ecdf(margin[,3]))
par(mfrow=c(2,1))
plot(ecdf10,seq(1/length(ecdf10),1,1/length(ecdf10)),type='l',main='Empirical Cumulative Distribution Function',xlab='Margins',ylab='ECDF')
lines(ecdf50,seq(1/length(ecdf50),1,1/length(ecdf50)),lty=2)
lines(ecdf100,seq(1/length(ecdf100),1,1/length(ecdf100)),lty=3)