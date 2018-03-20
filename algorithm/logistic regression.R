td0<-read.table('train_data.txt',head=F)
td0<-as.matrix(td0)
td<-cbind(rep(1,nrow(td0)),td0)
tl<-scan('train_labels.txt')
w1<-colMeans(td)
n<-.0001

tl <-y
td <- cbind(x1,x2[,-1])
for(i in 1:10000)
{
  w<-as.vector(w1+n*(t(tl-1/(1+exp(-td%*%w1)))%*%td-w1))
  w00 <- 1-.05/abs(w[-1])
  w00[w00<0|w00==Inf] <- 0
  w[-1] <- w00
  if(max(abs(w1-w))<.0001)
  {
    break()
  }else if(i==10000)
  {
    print("does not converge!")
  }
  w1<-w
}
ptd<-1/(1+exp(-td%*%w));ptd
predict.train<-ptd
predict.train[ptd<=.5]<-0
predict.train[ptd>.5]<-1
tst0<-read.table('test_data.txt',head=F)
tst0<-as.matrix(tst0)
tst<-cbind(rep(1,nrow(tst0)),tst0)
tsl<-scan("test_labels.txt")
ptst<-1/(1+exp(-tst%*%w));ptst
predict.test<-ptst
predict.test[ptst<=.5]<-0
predict.test[ptst>.5]<-1
error.train<-length(which(predict.train!=tl))/length(tl);error.train
error.test<-length(which(predict.test!=tsl))/length(tsl);error.test