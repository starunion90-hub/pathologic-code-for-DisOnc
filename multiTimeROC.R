
#install.packages("survival")
#install.packages("survminer")
#install.packages("timeROC")


library(survival)
library(survminer)
library(timeROC)

inputFile="indepInputVal.txt"      
outFile="ROCval.pdf"         
var="score"               
setwd("/Users/lixingchen/Desktop/HXH-TCGA/13.roc")      


rt=read.table(inputFile, header=T, sep="\t", check.names=F)

#绘制
ROC_rt=timeROC(T=rt$futime, delta=rt$fustat,
	           marker=rt[,var], cause=1,
	           weighting='aalen',
	           times=c(3,5,9), ROC=TRUE)
pdf(file=outFile,width=5,height=5)
plot(ROC_rt,time=3,col='green',title=FALSE,lwd=2)
plot(ROC_rt,time=5,col='blue',add=TRUE,title=FALSE,lwd=2)
plot(ROC_rt,time=9,col='red',add=TRUE,title=FALSE,lwd=2)
legend('bottomright',
	   c(paste0('AUC at 1 years: ',sprintf("%.03f",ROC_rt$AUC[3])),
	     paste0('AUC at 3 years: ',sprintf("%.03f",ROC_rt$AUC[5])),
	     paste0('AUC at 5 years: ',sprintf("%.03f",ROC_rt$AUC[9]))),
	     col=c("green",'blue','red'),lwd=2,bty = 'n')
dev.off()


######Video source: https://ke.biowolf.cn
######??????ѧ??: https://www.biowolf.cn/
######΢?Ź??ںţ?biowolf_cn
######???????䣺biowolf@foxmail.com
######????΢??: 18520221056
