#?趨?Լ??Ĺ???Ŀ¼
setwd("/Users/lixingchen/Desktop/HXH-TCGA/12.survival")

library(survival)

inputdata<- read.table("indepInputVal.txt",header=T,sep="\t")

#by risk
kms<-survfit(Surv(survival_time,status)~risk_level,data=inputdata)
kmdffexp=survdiff(Surv(survival_time,status)~risk_level,data=inputdata)
pValue=round(1-pchisq(kmdffexp$chisq,df=1),4)

pdf("survival_val.pdf")
plot(kms,lty="solid",col=c("blue","green","red"),
xlab="Survival time in years",ylab="Survival probabilities",
main=paste("Surival curve of risk score(P=", pValue ,")",sep=""))
legend("topright",c("Low score","Moderate score", "High score"),lty="solid",col=c("blue","green","red"))
dev.off()


