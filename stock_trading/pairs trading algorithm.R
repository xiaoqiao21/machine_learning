library(stockPortfolio)
library(ggplot2)
library(gtrendsR)
library(openxlsx)
attvz <- getReturns(c('T','VZ'), freq = "day",get = "all", start = "2012-10-03", end = "2013-10-04")
lrattvz <- attvz$R
lr <- lrattvz[rev(rownames(lrattvz)),]
pr0 <- cbind(attvz$fu$T$Adj.Close,attvz$fu$V$Ad)
rownames(pr0) <- attvz$full$T$Date
pr <- pr0[rev(rownames(pr0)),]
volattvz <- cbind(attvz$fu$T$Vol,attvz$fu$VZ$Vol)
rownames(volattvz) <- attvz$fu$T$Date
vol <- volattvz[rev(rownames(volattvz)),]
colnames(vol) <- c('T','VZ')
lvol <- log(vol)
plot(lr[,1])
qplot(1:nrow(lr),lr[,1],geom='smooth',main='AT&T')
qplot(1:nrow(lr),lr[,2],geom='smooth',main='Verizon')


atttr <- list()
atttr[[1]] <- gtrends("AT&T", start_date = "2012-10-04", end_date = "2012-12-04")$trend
atttr[[2]] <- gtrends("AT&T", start_date = "2012-12-05", end_date = "2013-02-04")$trend
atttr[[3]] <- gtrends("AT&T", start_date = "2013-02-05", end_date = "2013-04-04")$trend
atttr[[4]] <- gtrends("AT&T", start_date = "2013-04-05", end_date = "2013-06-04")$trend
atttr[[5]] <- gtrends("AT&T", start_date = "2013-06-05", end_date = "2013-08-04")$trend
atttr[[6]] <- gtrends("AT&T", start_date = "2013-08-05", end_date = "2013-11-04")$trend

atttrends <- do.call(rbind,atttr)
atred <- atttrends[!duplicated(atttrends[,1]),][-c(1:2,370:396),]
atred[,1] <- as.Date(atred[,1])
ind <- c()

for(i in 1:nrow(lr))
{
  ind[i] <- which(atred[,1]==rownames(lr)[i])
}

indd <- c(1,ind)
atreds <- atred[indd,]


vztr <- list()
vztr[[1]] <- gtrends("Verizon", start_date = "2012-10-04", end_date = "2012-12-04")$trend
vztr[[2]] <- gtrends("Verizon", start_date = "2012-12-05", end_date = "2013-02-04")$trend
vztr[[3]] <- gtrends("Verizon", start_date = "2013-02-05", end_date = "2013-04-04")$trend
vztr[[4]] <- gtrends("Verizon", start_date = "2013-04-05", end_date = "2013-06-04")$trend
vztr[[5]] <- gtrends("Verizon", start_date = "2013-06-05", end_date = "2013-08-04")$trend
vztr[[6]] <- gtrends("Verizon", start_date = "2013-08-05", end_date = "2013-11-04")$trend

vztrends <- do.call(rbind,vztr)
vzred <- vztrends[!duplicated(vztrends[,1]),][-c(1:2,370:396),]
vzred[,1] <- as.Date(vzred[,1])
vzreds <- vzred[indd,]

tren <- cbind(atreds[,2],vzreds[,2])
rownames(tren)[1] <- '2012-10-03'
rownames(tren)[-1] <- rownames(lr)

colnames(tren) <- c('AT&T','Verizon')
midp <- which(rownames(lr)=='2013-05-03')
vol1 <- lvol[-c(1,nrow(lvol)),]
tren1 <- tren[-c(1,nrow(tren)),]
lr1 <- lr[-nrow(lr),]
lr2 <- lr[-1,]
colnames(vol1) <- c('volT','volVZ')
colnames(tren1) <- c('trendsT','trendsVZ')
colnames(lr1) <- c('lrTt1','lrVZt1')
colnames(lr2) <- c('lrTt','lrVZt')
predic <- data.frame(cbind(lr2,lr1,vol1,tren1))
#m10 <- lm(lrTt~lrTt1+lrVZt1+lrTt1:lrVZt1+volT+volVZ+volT:volVZ+lrTt1:volT+lrVZt1:volVZ+trendsT+trendsVZ+trendsT:trendsVZ,data=predic[1:50,])
m11 <- lm(lrVZt~lrTt1+lrVZt1+lrTt1:lrVZt1+volT+volVZ+volT:volVZ+lrTt1:volT+lrVZt1:volVZ+trendsT+trendsVZ+trendsT:trendsVZ,data=predic[1:40,])
#m10s <- step(m10,direction='backward',k=log(nrow(vol1)))
m11s <- step(m11,direction='both',k=log(40))
#m20 <- lm(lrTt~lrTt1+lrVZt1+lrTt1:lrVZt1+volT+volVZ+lrTt1:volT+lrVZt1:volVZ+trendsT+trendsVZ+trendsT:trendsVZ,data=predic[51:100,])
m21 <- lm(lrVZt~lrTt1+lrVZt1+lrTt1:lrVZt1+volT+volVZ+lrTt1:volT+lrVZt1:volVZ+trendsT+trendsVZ+trendsT:trendsVZ,data=predic[41:80,])
#m20s <- step(m20,direction='backward',k=log(nrow(vol1)))
m21s <- step(m21,direction='both',k=log(40))
#m30 <- lm(lrTt~lrTt1+lrVZt1+lrTt1:lrVZt1+volT+volVZ+lrTt1:volT+lrVZt1:volVZ+trendsT+trendsVZ+trendsT:trendsVZ,data=predic[101:150,])
m31 <- lm(lrVZt~lrTt1+lrVZt1+lrTt1:lrVZt1+volT+volVZ+lrTt1:volT+lrVZt1:volVZ+trendsT+trendsVZ+trendsT:trendsVZ,data=predic[81:120,])
#m30s <- step(m30,direction='backward',k=log(nrow(vol1)))
m31s <- step(m31,direction='both',k=log(40))

line1 <- lm(lrVZt~lrVZt1+volVZ+trendsT+trendsVZ+lrVZt1:volVZ+trendsT:trendsVZ,data=predic[1:40,])
line2 <- lm(lrVZt~lrVZt1+volVZ+trendsT+trendsVZ+lrVZt1:volVZ+trendsT:trendsVZ,data=predic[41:80,])
line3 <- lm(lrVZt~lrVZt1+volVZ+trendsT+trendsVZ+lrVZt1:volVZ+trendsT:trendsVZ,data=predic[81:120,])


plot(predic[1:120,]$lrVZt,xlab='day',ylab='log return')
lines(1:40,predict(line1))
lines(41:80,predict(line2))
lines(81:120,predict(line3))


mlr <- mean(predic[1:120,]$lrVZt-predic[1:120,]$lrTt)
sdlr <- sd(predic[1:120,]$lrVZt-predic[1:120,]$lrTt)
money <- 100000
tes <- 30
num <- 0
bs <- c()
prpr <- c()
sta <- 121
for(i in sta:(sta+tes))
{
  tm <- lm(lrVZt~lrVZt1+volVZ+trendsT+trendsVZ+lrVZt1:volVZ+trendsT:trendsVZ,data=predic[(i-30):i,])
  prlr <- predict(tm,predic[i,])
  prpr[i-sta+1] <- prlr
  if(predic[i,]$lrVZt1-predic[i,]$lrTt1<mlr-2*sdlr&money[i-sta+1]>100)
  {
    bs[i-sta+1] <- floor(money[i-sta+1]/pr[i+2,2])
    num[i-sta+2] <- num[i-sta+1]+bs[i-sta+1]
    money[i-sta+2] <- money[i-sta+1]-bs[i-sta+1]*pr[i+2,2]
  }else if(predic[i,]$lrVZt1-predic[i,]$lrTt1<mlr-sdlr&prlr>0&money[i-sta+1]>100)
  {
    bs[i-sta+1] <- floor(money[i-sta+1]*.5/pr[i+2,2])
    num[i-sta+2] <- num[i-sta+1]+bs[i-sta+1]
    money[i-sta+2] <- money[i-sta+1]-bs[i-sta+1]*pr[i+2,2]
  }else if(predic[i,]$lrVZt1-predic[i,]$lrTt1>mlr+2*sdlr&num[i-sta+1]>0)
  {
    bs[i-sta+1] <- -num[i-sta+1]
    num[i-sta+2] <- 0
    money[i-sta+2] <- money[i-sta+1]-bs[i-sta+1]*pr[i+2,2]
  }else if(predic[i,]$lrVZt1-predic[i,]$lrTt1>mlr+sdlr&prlr<0&num[i-sta+1]>0)
  {
    bs[i-sta+1] <- -floor(num[i-sta+1]*.5)
    num[i-sta+2] <- num[i-sta+1]+bs[i-sta+1]
    money[i-sta+2] <- money[i-sta+1]-bs[i-sta+1]*pr[i+2,2]
  }else
  {
    bs[i-sta+1] <- 0
    num[i-sta+2] <- num[i-sta+1]
    money[i-sta+2] <- money[i-sta+1]
  }
  if(i==sta+tes&num[i-sta+1]>0)
  {
    bs[i-sta+1] <- -num[i-sta+1]
    num[i-sta+2] <- 0
    money[i-sta+2] <- money[i-sta+1]-bs[i-sta+1]*pr[i+2,2]
  }
}
money

plot(predic[sta:(sta+30),]$lrVZt1,xlab='day',ylab='log return')
lines(prpr,col='red')
for(i in 1:31)
{
  if(bs[i]>0)
  {
    abline(v=i,col='green')
  }
  if(bs[i]<0)
  {
    abline(v=i,col='blue')
  }
}