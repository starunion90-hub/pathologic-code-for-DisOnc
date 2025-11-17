#???ذ?װ??
library(rms)
library(foreign)
library(survival)

#?޸??Լ??Ĺ???Ŀ¼
setwd("/Users/lixingchen/Desktop/HXH-TCGA/11.nomogram")
#??ȡ????
seer<-read.table("indepInput.txt",header=T,sep="\t")

#??????ת???????Ӹ?ʽ 
seer$grade<-factor(seer$grade,labels=c("G1","G2","G3"))
seer$stage<-factor(seer$stage,labels=c("StageI","StageII","StageIII","StageIV"))
seer$risk<-factor(seer$risk,labels=c("Low","High"))
seer$TCGAsubtype<-factor(seer$TCGAsubtype,labels=c("POLE","MSI","CNL","CNH"))


#?????ݴ?????
ddist <- datadist(seer)
options(datadist='ddist')

#?????????ص?Cox?ع?ģ??
cox <- cph(Surv(survival_time,status) ~stage + grade  + TCGAsubtype + risk ,surv=T,x=T, y=T,data=seer) 
surv <- Survival(cox)

surv <- Survival(cox)
sur_3_year<-function(x)surv(1*365*3,lp=x)#3??????
sur_5_year<-function(x)surv(1*365*5,lp=x)#5??????
sur_7_year<-function(x)surv(1*365*7,lp=x)#5??????
nom_sur <- nomogram(cox,fun=list(sur_3_year,sur_5_year,sur_7_year),lp= F,funlabel=c('1-Year Survival','3-Year survival','5-Year survival'),maxscale=100,fun.at=c('0.9','0.8','0.7','0.6','0.5','0.4','0.3','0.2','0.1'))

#??????ͼ
pdf("nomogram2.pdf")
plot(nom_sur,xfrac=0.25)
dev.off()

#c-index
#?????????ص?Cox?ع?ģ??
fmla1 <- as.formula(Surv(survival_time,status) ~ Grade + tumorResidual  + risk)
cox2 <- cph(fmla1,data=seer)
#????C-index
coxpe <- predict(cox2)#ģ??Ԥ??
c_index=1-rcorr.cens(coxpe,Surv(seer$survival_time,seer$status))
c_index
