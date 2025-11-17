#?趨?Լ??Ĺ???Ŀ¼
setwd("/Users/jinjiayang/Desktop/ECmodel/LNM/12.nomogram")
#???ذ?װ??
library(rms)
library(foreign)
library(survival)

#??ȡ????
seer<-read.table("indepInput2.txt",header=T,sep="\t")
 
seer$age<-factor(seer$age,labels=c("<60",">=60"))
seer$grade<-factor(seer$grade,labels=c("G1","G2","G3"))
seer$stage<-factor(seer$stage,labels=c("stageI","stageII","stageIII","stageIV"))
seer$risk<-factor(seer$risk,labels=c("low","high"))
#??????????Cox?ع?ģ??
#3-year
cox1 <- cph(Surv(survival_time,status) ~ Grade + tumorResidual  + risk,surv=T,x=T, y=T,time.inc = 1*365*2,data=seer) 

cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*365*2, m= 80, B=100)

#??У׼ͼ
pdf("calibrate2.pdf",12,8)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 2-Year Survival",ylab="Actual 2-Year Survival",col="blue")
lines(cal,c('mean.predicted',??KM'),type = ??a',lwd = 3,col ="black" ,pch = 16)
box(lwd = 1)
abline(0,1,lty = 3,lwd = 3,col = "black")
dev.off()

#5-year
cox1 <- cph(Surv(survival_time,status) ~ age + CancerStatus + tumorResidual  + risk,surv=T,x=T, y=T,time.inc = 1*365*5,data=seer) 

cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*365*7, m= 80, B=100)

#??У׼ͼ
pdf("calibrate5.pdf",12,8)
par(mar = c(10,5,3,2),cex = 1.0)
plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 3-Year Survival",ylab="Actual 3-Year Survival",col="red")
lines(cal,c('mean.predicted',??KM'),type = ??a',lwd = 3,col ="black" ,pch = 16)
      box(lwd = 1)
      abline(0,1,lty = 3,lwd = 3,col = "black")
      dev.off()

      #7-year
      cox1 <- cph(Surv(survival_time,status) ~ age + stage + grade  + risk,surv=T,x=T, y=T,time.inc = 1*365*7,data=seer) 
      
      cal <- calibrate(cox1, cmethod="KM", method="boot", u=1*365*7, m= 150, B=100)
      
      #??У׼ͼ
      pdf("calibrate7.pdf",12,8)
      par(mar = c(10,5,3,2),cex = 1.0)
      plot(cal,lwd=3,lty=2,errbar.col="black",xlim = c(0,1),ylim = c(0,1),xlab ="Nomogram-Predicted Probability of 3-Year Survival",ylab="Actual 3-Year Survival",col="red")
      lines(cal,c('mean.predicted',??KM'),type = ??a',lwd = 3,col ="black" ,pch = 16)
            box(lwd = 1)
            abline(0,1,lty = 3,lwd = 3,col = "black")
            dev.off()