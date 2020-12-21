#PCA 분석
secu_com_finance_2007 <- read.csv("C:/Users/user/Desktop/밀화부리/R/secu_com_finance_2007.csv",
                                  header=TRUE, stringsAsFactors=FALSE)
head(secu_com_finance_2007)
#v1 : 총자본순이익률
#v2 : 자기자본순이익률
#v3 : 자기자본비율
#v4 : 부채비율
#v5 : 자기자본회전율
secu_com_finance_2007 <- transform(secu_com_finance_2007,
                                   V1_s=scale(V1),
                                   V2_s=scale(V2),
                                   V3_s=scale(V3),
                                   V4_s=scale(V4),
                                   V5_s=scale(V5))
secu_com_finance_2007 <- transform(secu_com_finance_2007,
                                   V4_s2=max(V4_s)-V4_s)
secu_com_finance_2007_2 <- secu_com_finance_2007[,c("company","V1_s","V2_s","V3_s","V4_s2","V5_s")]
cor(secu_com_finance_2007_2[,-1])
round(cor(secu_com_finance_2007_2[,-1]))
plot(secu_com_finance_2007_2[,-1])

secu_prcomp <- prcomp(secu_com_finance_2007_2[,c(2:6)])
summary(secu_prcomp)
print(secu_prcomp)

plot(prcomp(secu_com_finance_2007_2[,c(2:6)]), type="l",
     sub = "Scree Plot")

biplot(prcomp(secu_com_finance_2007_2[,c(2:6)]),cex=c(0.7,0.8))     
secu_pc1 <- predict(secu_prcomp)[,1]
secu_pc2 <- predict(secu_prcomp)[,2]
text(secu_pc1,secu_pc2,labels=secu_com_finance_2007_2$company,
     cex=0.7,pos=3,col="blue")
pca <- function(dataset){
  pc = prcomp(dataset,scale. = TRUE)
  
  k=0
  R=0
  while(R<0.8){
    k=k+1
    R=sum(pc[[1]][1:k]^2)/sum(pc[[1]]^2)
    
    cat("When number of Principal Component(k) is ",k,
        ", Cumulative Proportion(R) is ",R,"\n","\n",sep="")
  }
  SelectedDataSet=pc[[5]][,1:k]
  return(SelectedDataSet)
}
pca(secu_com_finance_2007_2[,c(2:6)])

#요인분석

secu_com_finance_2007 <- read.csv("C:/Users/user/Desktop/밀화부리/R/secu_com_finance_2007.csv",
                                  header=TRUE, stringsAsFactors=FALSE)
secu_com_finance_2007 <- transform(secu_com_finance_2007,
                                   V1_s=scale(V1),
                                   V2_s=scale(V2),
                                   V3_s=scale(V3),
                                   V4_s=scale(V4),
                                   V5_s=scale(V5))
secu_com_finance_2007 <- transform(secu_com_finance_2007,
                                   V4_s2=max(V4_s)-V4_s)
secu_com_finance_2007_2 <- secu_com_finance_2007[,c("company","V1_s","V2_s","V3_s","V4_s2","V5_s")]
cor(secu_com_finance_2007_2[,-1])
round(cor(secu_com_finance_2007_2[,-1]))
plot(secu_com_finance_2007_2[,-1])

secu_factanal <- factanal(secu_com_finance_2007_2[,2:6],
                          factors=2,
                          rotation="varimax",
                          scores="regression")
print(secu_factanal)
print(secu_factanal$loadings,cutoff=0)
secu_factanal$scores

plot(secu_factanal$scores,main="Biplot of the first 2 factors")
text(secu_factanal$score[,1],secu_factanal$scores[,2],
     labels=secu_com_finance_2007_2$company,
     cex=0.7,pos=3,col="blue")
points(secu_factanal$loadings,pch=19,col="red")
text(secu_factanal$loadings[,1],secu_factanal$loadings[,2],
     labels=rownames(secu_factanal$loadings),
     cex=0.8,pos=3,col="red")

segments(0,0,secu_factanal$loadings[1,1], secu_factanal$loadings[1,2])
segments(0,0,secu_factanal$loadings[2,1], secu_factanal$loadings[2,2])
segments(0,0,secu_factanal$loadings[3,1], secu_factanal$loadings[3,2])
segments(0,0,secu_factanal$loadings[4,1], secu_factanal$loadings[4,2])
segments(0,0,secu_factanal$loadings[5,1], secu_factanal$loadings[5,2])


x <- seq(0,2*pi,by=pi/100)
amp.1 <- 2 #진폭 2
amp.2 <- 2
amp.3 <- 5
amp.4 <- 5

wav.1 <- 1 #주기 1
wav.2 <- 2
wav.3 <- 3
wav.4 <- 7

signal.1 <- amp.1*sin(wav.1*x) #진폭 2 & 주기 1인 사인함수
signal.2 <- amp.2*sin(wav.2*x) #진폭 2 & 주기 2인 사인함수
signal.3 <- amp.3*sin(wav.3*x) #진폭 5 & 주기 3인 사인함수
signal.4 <- amp.4*sin(wav.4*x) #진폭 5 & 주기 7인 사인함수

par(mfrow=c(1,4))
plot(x,signal.1,type='l',ylim=c(-5,5)); abline(h=0,lty=3)
plot(x,signal.2,type='l',ylim=c(-5,5)); abline(h=0,lty=3)
plot(x,signal.3,type='l',ylim=c(-5,5)); abline(h=0,lty=3)
plot(x,signal.4,type='l',ylim=c(-5,5)); abline(h=0,lty=3)

signal.1234 <- signal.1+signal.2+signal.3+signal.4
head(signal.1234,n=30)

par(mfrow=c(1,1))
plot(x,signal.1234,type='l',main="Sum of siganl.1&2&3&4",
     xlab="Time",ylab="Amplitude")
abline(h=0,lty=3)

library(stats)
N <- length(x)
fft_x_abs <- abs(fft(signal.1234)/((N-1)/2))
plot(fft_x_abs,type = "h")
plot(fft_x_abs[2:20],type = 'h')
#index 1이 0hz이기때문에 인덱스를 2부터 시작한다.