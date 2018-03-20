library(openxlsx)
library(quantmod)
library(huge)
library(qgraph)
d <- read.xlsx('5_min_data.xlsx')
lr <- apply(d[,-1],2,ROC)
lr2 <- lr[-c(1,nrow(lr)),]
ma <- apply(lr2,2,runMean,n=12)[-(1:11),]
acf(ma[,1])

ha <- huge(huge.npn(lr), method="glasso")
ad <- ha$path[[2]]
ajac <- apply(ad,2,as.numeric)
cona <- list()
k <- 1
for(i in 2:nrow(ajac))
{
  for(j in 1:(i-1))
  {
    if(ajac[i,j]==1)
    {
      cona[[k]] <- c(colnames(lr)[i],colnames(lr)[j])
      k <- k+1
    }
  }
}
conname <- do.call(rbind,cona)
attvz <- cbind(lr2[,84],lr2[,93])
mt1 <- attvz[seq(1,nrow(attvz),10),]
mt <- attvz[seq(2,nrow(attvz),10),]
colnames(mt1) <- c('ATT t-1','VZ t-1')
colnames(mt) <- c('ATT t','VZ t')
ml <- lm(mt~mt1[1:929])

cormat <- cor_auto(lr)
qgraph(cormat,graph='glasso',tuning=.5,sampleSize=nrow(lr))