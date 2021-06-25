library(MASS)
library(survival)
library(glmnet)
library(randomForestSRC)
library(sas7bdat)

####data structure (Z,X,Y,D)
####cens can take value "random", "Z", "X", "ZX"
####model can take value "Cox", "Lasso", "RF"
myest<-function(data,t0,model,cens="random"){
	n0=sum(data$Z==0)
	n1=sum(data$Z==1)
	n=n0+n1
	p=ncol(data)-3
	if (cens=="random"){
		fitc=coxph(Surv(Y,1-D)~1,data=data[,c("Y","D")])
		newdatac=data[,c("Y","D")]
		newdatac$Y=apply(cbind(data$Y,t0),1,min)
		mysurv=stepfun(survfit(fitc)$time,c(1,survfit(fitc)$surv))
		pic=mysurv(newdatac$Y)
	}
	if (cens=="Z"){
		fitc=coxph(Surv(Y,1-D)~Z,data=data[,c("Y","D","Z")])
		newdatac=data[,c("Y","D","Z")]
		newdatac$Y=apply(cbind(data$Y,t0),1,min)
		pic=exp(-predict(fitc,newdatac,type="expected"))
	}
	if (cens=="X"){
		fitc=coxph(Surv(Y,1-D)~.,data=data[,-1])
		newdatac=data[,-1]
		newdatac$Y=apply(cbind(data$Y,t0),1,min)
		fitc_s=stepAIC(fitc)
		pic=exp(-predict(fitc_s,newdatac,type="expected"))
	}	
	if (cens=="ZX"){
		fitc=coxph(Surv(Y,1-D)~.,data=data)
		newdatac=data
		newdatac$Y=apply(cbind(data$Y,t0),1,min)
		fitc_s=stepAIC(fitc)
		pic=exp(-predict(fitc_s,newdatac,type="expected"))
	}
	if (model=="Cox"){
		fit1=coxph(Surv(Y,D)~.,data=data[which(data$Z==1),-1])
		fit0=coxph(Surv(Y,D)~.,data=data[which(data$Z==0),-1])
		newdata0=data[,-1]
		newdata1=data[,-1]
		newdata0$Y=t0
		newdata1$Y=t0
		p1=exp(-predict(fit1,newdata1,type="expected"))
		p0=exp(-predict(fit0,newdata0,type="expected"))			
	}
	if (model=="Lasso"){
		gfit1=cv.glmnet(as.matrix(data[which(data$Z==1),2:(1+p)]),Surv(data$Y[which(data$Z==1)],data$D[which(data$Z==1)]),family="cox")
		a1=which(coef(gfit1,s="lambda.min")!=0)
		if (length(a1)==0){
			fit1=survfit(Surv(Y,D)~1,data=data[which(data$Z==1),c(p+2,p+3)])
			s1=stepfun(fit1$time,c(1,fit1$surv))
			p1=rep(s1(t0),n0+n1)
		}
		if (length(a1)>0){
			fit1=coxph(Surv(Y,D)~.,data=data[which(data$Z==1),c(1+a1,p+2,p+3)])
			newdata1=data[,c(1+a1,p+2,p+3)]
			newdata1$Y=t0
			p1=exp(-predict(fit1,newdata1,type="expected"))
		}
		gfit0=cv.glmnet(as.matrix(data[which(data$Z==0),2:(1+p)]),Surv(data$Y[which(data$Z==0)],data$D[which(data$Z==0)]),family="cox")
		a0=which(coef(gfit0,s="lambda.min")!=0)
		if (length(a0)==0){
			fit0=survfit(Surv(Y,D)~1,data=data[which(data$Z==0),c(p+2,p+3)])
			s0=stepfun(fit0$time,c(1,fit0$surv))
			p0=rep(s0(t0),n0+n1)
		}
		if (length(a0)>0){
			fit0=coxph(Surv(Y,D)~.,data=data[which(data$Z==0),c(1+a0,p+2,p+3)])
			newdata0=data[,c(1+a0,p+2,p+3)]
			newdata0$Y=t0
			p0=exp(-predict(fit0,newdata0,type="expected"))
		}
	}
	if (model=="RF"){
		gfit1= rfsrc(Surv(Y, D) ~ ., data = data[which(data$Z==1),-1], nsplit=10,ntree = 200,tree.err=TRUE)
		newdata1=data[,-1]
		newdata1$Y=t0	
		pred1=predict(gfit1,newdata1)
		p1=p0=rep(NA,n0+n1)
		for (i in 1:(n0+n1)){
			s1=stepfun(pred1$time.interest,c(1,pred1$survival[i,]))
			p1[i]=s1(t0)
		}
		gfit0= rfsrc(Surv(Y, D) ~ ., data = data[which(data$Z==0),-1], nsplit=10,ntree = 200,tree.err=TRUE)
		newdata0=data[,-1]
		newdata0$Y=t0	
		pred0=predict(gfit0,newdata0)
		for (i in 1:(n0+n1)){
			s0=stepfun(pred0$time.interest,c(1,pred0$survival[i,]))
			p0[i]=s0(t0)
		}
	}
	R=as.numeric((data$Y>t0)|data$D)
	y_weight=R/pic*as.numeric(data$Y>t0)
	tauhat0=mean(y_weight[which(data$Z==1)])-mean(y_weight[which(data$Z==0)])
	tauhat1=mean(p1-p0)
	d0=R*(as.numeric(data$Y>t0)-p0)/pic
	d1=R*(as.numeric(data$Y>t0)-p1)/pic
	tauhat2=mean(R*p1/pic-R*p0/pic)+mean(d1[which(data$Z==1)])-mean(d0[which(data$Z==0)])
	e1=R*(as.numeric(data$Y>t0)-p1)/pic
	e0=R*(as.numeric(data$Y>t0)-p0)/pic
	tauhat3=mean(p1-p0)+mean(e1[which(data$Z==1)])-mean(e0[which(data$Z==0)])
	c(tauhat0,tauhat1,tauhat2,tauhat3)
}
####read in data and create dummy variables
fulldata<-read.sas7bdat("matched_cr1_2.sas7bdat")
fulldata$condtbi2<-as.numeric(fulldata$condtbi==2)
fulldata$condtbi3<-as.numeric(fulldata$condtbi==3)
fulldata$yeargp5<-as.numeric(fulldata$yeargp==5)
fulldata$yeargp6<-as.numeric(fulldata$yeargp==6)
fulldata$gvhdgpc3<-as.numeric(fulldata$gvhdgpc==3)
fulldata$gvhdgpc4<-as.numeric(fulldata$gvhdgpc==4)
fulldata$wbcdxgp2<-as.numeric(fulldata$wbcdxgp==2)
fulldata$wbcdxgp3<-as.numeric(fulldata$wbcdxgp==3)
fulldata$wbcdxgp99<-as.numeric(fulldata$wbcdxgp==99)


for (t0 in c(6,12,18,24,30,36,42,48,54,60)){
	####short list
	data<-fulldata[,c("murd","condtbi2","condtbi3","yeargp5","yeargp6","axdel1q","axtrisx","axt411","intxsurv","dead")]
	names(data)<-c("Z","X1","X2","X3","X4","X5","X6","X7","Y","D")
	M<-100
	result<-matrix(data=NA,nrow=1+M,ncol=18)
	onesim<-function(data){
		est1<-est2<-est3<-est4<-rep(NA,18)
		try(est1<-c(myest(data,t0,"Cox"),myest(data,t0,"Lasso"),myest(data,t0,"RF")))
		est1
	}
	result[1,]<-onesim(data)
	for (i in 2:(M+1)){
		set.seed(5555+i)
		bb<-sample(1:nrow(data),nrow(data),replace=TRUE)
		bdata=data[bb,]
		result[i,]<-onesim(bdata)
	}
	write.csv(result,paste("t=",as.character(t0),"result-short.csv",sep=""))



	####medium list
	data<-fulldata[,c("murd","condtbi2","condtbi3","kps","gvhdgpc3","gvhdgpc4","wbcdxgp2","wbcdxgp3","wbcdxgp99","graftype","age","yeargp5","yeargp6","axdel1q","axtrisx","axt411","intxsurv","dead")]
	names(data)<-c("Z","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","Y","D")
	M<-100
	result<-matrix(data=NA,nrow=1+M,ncol=18)
	onesim<-function(data){
		est1<-est2<-est3<-est4<-rep(NA,18)
		try(est1<-c(myest(data,t0,"Cox"),myest(data,t0,"Lasso"),myest(data,t0,"RF")))
		est1
	}
	result[1,]<-onesim(data)
	for (i in 2:(M+1)){
		set.seed(5555+i)
		bb<-sample(1:nrow(data),nrow(data),replace=TRUE)
		bdata=data[bb,]
		result[i,]<-onesim(bdata)
	}
	write.csv(result,paste("t=",as.character(t0),"result-medium.csv",sep=""))



	####long list
	data<-fulldata[,c("murd","condtbi2","condtbi3","kps","gvhdgpc3","gvhdgpc4","wbcdxgp2","wbcdxgp3","wbcdxgp99","graftype","age","yeargp5","yeargp6","add5qdel5q","add12pdel12p","del7qm7","m17i17qdel17p","add7pi7q", "tratriphyper","lohyperpo", "add9pdel9p", "axt119", "axt411", "axt1011", "axt1119", "axdel6q", "axdel11q", "axtris8", "axmk", "axcomplex", "axm13", "axtris6", "axtris10", "axtris22", "axtrisx","axadd14q32", "axdel1q", "axdel17p","intxsurv","dead")]
	names(data)<-c("Z","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22","X23","X24","X25","X26","X27","X28","X29","X30","X31","X32","X33","X34","X35","X36","X37","Y","D")
	M<-100
	result<-matrix(data=NA,nrow=1+M,ncol=18)
	onesim<-function(data){
		est1<-est2<-est3<-est4<-rep(NA,18)
		try(est1<-c(myest(data,t0,"Cox"),myest(data,t0,"Lasso"),myest(data,t0,"RF")))
		est1
	}
	result[1,]<-onesim(data)
	for (i in 2:(M+1)){
		set.seed(5555+i)
		bb<-sample(1:nrow(data),nrow(data),replace=TRUE)
		bdata=data[bb,]
		result[i,]<-onesim(bdata)
	}
	
	write.csv(result,paste("t=",as.character(t0),"result-long.csv",sep=""))
}



#####summarize result short
estmat_s<-matrix(data=NA,nrow=11,ncol=12)

semat_s<-matrix(data=NA,nrow=11,ncol=12)
setwd("C:/Users/zhengc/Desktop/survival-causal")
sdata<-matrix(data=0,nrow=101,ncol=12)

for (i in 1:10){

t0<-6*i

filename<-paste("t=",as.character(t0),"result-short.csv",sep="")
data<-as.matrix(read.csv(filename)[,-1])

estmat_s[i,]<-as.vector(data[1,])

semat_s[i,]<-as.vector(apply(data[-1,],2,sd,na.rm=TRUE))

sdata<-sdata+data

}
estmat_s[11,]<-sdata[1,]/10

semat_s[11,]<-apply(sdata[-1,],2,sd,na.rm=TRUE)/10

write.csv(estmat_s,"summary-est-short.csv")

write.csv(semat_s,"summary-se-short.csv")



#####summarize result medium
estmat_m<-matrix(data=NA,nrow=11,ncol=12)

semat_m<-matrix(data=NA,nrow=11,ncol=12)

sdata<-matrix(data=0,nrow=101,ncol=12)

for (i in 1:10){

t0<-6*i

filename<-paste("t=",as.character(t0),"result-medium.csv",sep="")

data<-as.matrix(read.csv(filename)[,-1])

estmat_m[i,]<-as.vector(data[1,])

semat_m[i,]<-as.vector(apply(data[-1,],2,sd,na.rm=TRUE))

sdata<-sdata+data

}

estmat_m[11,]<-sdata[1,]/10

semat_m[11,]<-apply(sdata[-1,],2,sd,na.rm=TRUE)/10

write.csv(estmat_m,"summary-est-medium.csv")

write.csv(semat_m,"summary-se-medium.csv")


#####summarize result long
estmat_l<-matrix(data=NA,nrow=11,ncol=12)

semat_l<-matrix(data=NA,nrow=11,ncol=12)

sdata<-matrix(data=0,nrow=101,ncol=12)

for (i in 1:10){

t0<-6*i

filename<-paste("t=",as.character(t0),"result-long.csv",sep="")

data<-as.matrix(read.csv(filename)[,-1])

estmat_l[i,]<-as.vector(data[1,])

semat_l[i,]<-as.vector(apply(data[-1,],2,sd,na.rm=TRUE))

sdata<-sdata+data

}

estmat_l[11,]<-sdata[1,]/10

semat_l[11,]<-apply(sdata[-1,],2,sd,na.rm=TRUE)/10

write.csv(estmat_l,"summary-est-long.csv")

write.csv(semat_l,"summary-se-long.csv")


pdf("Result-Plots.pdf")

k<-0

tt<-(1:10)*6

plot(estmat_s[1:10,k+1]~tt,ylim=c(-0.25,0.14),type="l",xlab="ACE(t)",ylab="t",main="")

lines((estmat_s[1:10,k+1]+1.96*semat_s[1:10,k+1])~tt,lty=2)

lines((estmat_s[1:10,k+1]-1.96*semat_s[1:10,k+1])~tt,lty=2)

lines(estmat_s[1:10,k+4]~tt,col="red")

lines((estmat_s[1:10,k+4]+1.96*semat_s[1:10,k+4])~tt,lty=2,col="red")

lines((estmat_s[1:10,k+4]-1.96*semat_s[1:10,k+4])~tt,lty=2,col="red")

k<-8


lines(estmat_l[1:10,k+6]~tt,col="blue")

lines((estmat_l[1:10,k+6]+1.96*semat_l[1:10,k+6])~tt,lty=2,col="blue")

lines((estmat_l[1:10,k+6]-1.96*semat_l[1:10,k+6])~tt,lty=2,col="blue")


abline(0,0)




###create average table

tab_s<-cbind(estmat_s[11,1:4],semat_s[11,1:4],2*pnorm(-abs(estmat_s[11,1:4]/semat_s[11,1:4])),estmat_s[11,5:8],semat_s[11,5:8],2*pnorm(-abs(estmat_s[11,5:8]/semat_s[11,5:8])),estmat_s[11,9:12],semat_s[11,9:12],2*pnorm(-abs(estmat_s[11,9:12]/semat_s[11,9:12])))

tab_m<-cbind(estmat_m[11,1:4],semat_m[11,1:4],2*pnorm(-abs(estmat_m[11,1:4]/semat_m[11,1:4])),estmat_m[11,5:8],semat_m[11,5:8],2*pnorm(-abs(estmat_m[11,5:8]/semat_m[11,5:8])),estmat_m[11,9:12],semat_m[11,9:12],2*pnorm(-abs(estmat_m[11,9:12]/semat_m[11,9:12])))

tab_l<-cbind(estmat_l[11,1:4],semat_l[11,1:4],2*pnorm(-abs(estmat_l[11,1:4]/semat_l[11,1:4])),estmat_l[11,5:8],semat_l[11,5:8],2*pnorm(-abs(estmat_l[11,5:8]/semat_l[11,5:8])),estmat_l[11,9:12],semat_l[11,9:12],2*pnorm(-abs(estmat_l[11,9:12]/semat_l[11,9:12])))

write.csv(rbind(tab_s,tab_m,tab_l),"table.csv")

