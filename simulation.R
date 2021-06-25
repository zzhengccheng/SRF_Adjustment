library(MASS)
library(survival)
library(glmnet)
library(randomForestSRC)
library(sas7bdat)

simulate<-function(n,pi1,p,beta,s0,s1,rho,tau,seed,sp){
	set.seed(1111)
	Sigma=rho^abs(outer(1:p,1:p,"-"))
	Xt=matrix(rnorm(20*p),nrow=20,ncol=p)%*%chol(Sigma)
	X=kronecker(matrix(data=1,nrow=n/20,ncol=1),Xt)
	gamma0=s0/(1:p)^2
	gamma1=s1/(1:p)^2
	if (p>sp){
		gamma0[(1+sp):p]=0
		gamma1[(1+sp):p]=0
	}
	set.seed(seed)
	Z=as.numeric(runif(n)<pi1)
	T0=-log(runif(n))/exp(X%*%gamma0)
	T1=-log(runif(n))/exp(beta+X%*%gamma1)
	C=runif(n)*tau
	T=Z*T1+(1-Z)*T0
	Y=apply(cbind(C,T),1,min)
	D=as.numeric(T<=C)
	data.frame(Z,X,Y,D)
}

truepar<-function(n,pi1,p,beta,s0,s1,rho,tau,t0,sp){
	set.seed(1111)
	Sigma=rho^abs(outer(1:p,1:p,"-"))
	X=mvrnorm(20,mu=rep(0,p),Sigma=Sigma)
	X=kronecker(matrix(data=1,nrow=n/20,ncol=1),X)
	gamma0=s0/(1:p)^2
	gamma1=s1/(1:p)^2
	if (p>sp){
		gamma0[(sp+1):p]=0
		gamma1[(sp+1):p]=0
	}
	Z=as.numeric(runif(n)<pi1)
	T0=-log(runif(n))/exp(X%*%gamma0)
	T1=-log(runif(n))/exp(beta+X%*%gamma1)
	C=runif(n)*tau
	T=Z*T1+(1-Z)*T0
	Y=apply(cbind(C,T),1,min)
	D=as.numeric(T<=C)
	mean(as.numeric(T1>t0)-as.numeric(T0>t0))
}

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
	e1=R*(as.numeric(data$Y>t0)-p1)
	e0=R*(as.numeric(data$Y>t0)-p0)
	tauhat3=mean(p1-p0)+mean(e1[which(data$Z==1)])-mean(e0[which(data$Z==0)])
	c(tauhat0,tauhat1,tauhat2,tauhat3)
}

fullest<-function(data,t0,model,cens="random",M){
	est_tau<-myest(data,t0,model,cens)
	est_mat<-matrix(data=NA,nrow=M,ncol=length(est_tau))
	for (i in 1:M){
		set.seed(8888+i)
		rr<-sample(1:nrow(data),nrow(data),replace=TRUE)
		est_mat[i,]<-myest(data[rr,],t0,model,cens)
	}
	c(est_tau,apply(est_mat,2,sd,na.rm=TRUE))
}

onesim<-function(seed,n,pi1,p,beta,s0,s1,rho,tau,t0,true_tau,sp){
	data<-simulate(n,pi1,p,beta,s0,s1,rho,tau,seed,sp)
	result<-matrix(data=NA,nrow=6,ncol=8)
	count<-0
	for (model in c("Cox","Lasso","RF")){
		for (cens in c("random","Z")){
			count<-count+1
			try(result[count,]<-fullest(data,t0,model,cens,20))
		}
	}
	est<-as.vector(t(result[,1:4]))
	se<-as.vector(t(result[,5:8]))
	c(est,se,abs(est/se),abs(est/se)>1.96,abs(est-true_tau)/se<1.96)
}
#1:24 est
#25:48 se
#49:72 ratio
#73:96 power
#97:120 CR

####n: sample size (need to be multiplier of 20)
####pi1: Pr(Z=1)
####p: number of parameters (need to be greater than 10)
####sparsity currently setting at 10
####beta: Coefficient for Z (0 with s0=s1 means no effect)
####s0: Effect of X on Z=0
####s1: Effect of X on Z=1
####rho: Correlation between X columns (AR structure)
####tau: control censoring probability


fullsim<-function(n,pi1,p,beta,s0,s1,rho,tau,sp){
	set.seed(1111)
	data<-simulate(50000,pi1,p,beta,s0,s1,rho,tau,1111,sp)
	t0<-quantile(data$Y,probs=0.5,na.rm=TRUE)
	filename<-paste("n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	true_tau<-truepar(10000,pi1,p,beta,s0,s1,rho,tau,t0,sp)
	allres<-sapply(8888+(1:100),onesim,n=n,pi1=pi1,p=p,beta=beta,s0=s0,s1=s1,rho=rho,tau=tau,t0=t0,true_tau=true_tau,sp=sp)
	allres<-t(allres)
	write.csv(allres,paste("Allres-",filename,sep=""))
	Bias<-apply(allres[,1:24],2,mean,na.rm=TRUE)-true_tau
	SD<-apply(allres[,1:24],2,sd,na.rm=TRUE)
	ESE<-apply(allres[,25:48],2,mean,na.rm=TRUE)
	MSE<-Bias^2+SD^2
	zvalue<-apply(allres[,49:72],2,median,na.rm=TRUE)
	power<-apply(allres[,73:96],2,mean,na.rm=TRUE)
	CR<-apply(allres[,97:120],2,mean,na.rm=TRUE)
	RelMSE<-MSE/rep(MSE[c(1,5,9,13,17,21)],each=4)
	largerz<-apply(allres[,49:72]-kronecker(allres[,48+c(1,5,9,13,17,21)],matrix(1,1,4))>0,2,mean,na.rm=TRUE)
	write.csv(cbind(Bias,SD,ESE,MSE,RelMSE,zvalue,largerz,power,CR),paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep=""),row.names=FALSE)
}



data<-simulate(50000,pi1,p,beta,s0,s1,rho,tau,1111,sp)
mean(data$D) #0.7

####setting (p,k)=(10,10)
n=100
pi1=0.5
p=10
sp=10
rho=0.8
tau=2.5
s0=1
s1=1
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	fullsim(n,pi1,p,beta,s0,s1,rho,tau,sp)
}
beta=0.5
for (s1 in c(0,0.2,0.4,0.6,0.8,1)){
	fullsim(n,pi1,p,beta,s1,s1,rho,tau,sp)
	fullsim(n,pi1,p,beta,0,s1,rho,tau,sp)
}

####setting (p,k)=(50,10)
n=100
pi1=0.5
p=50
sp=10
rho=0.8
tau=2.5
s0=1
s1=1
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	fullsim(n,pi1,p,beta,s0,s1,rho,tau,sp)
}
beta=0.5
for (s1 in c(0,0.2,0.4,0.6,0.8,1)){
	fullsim(n,pi1,p,beta,s1,s1,rho,tau,sp)
	fullsim(n,pi1,p,beta,0,s1,rho,tau,sp)
}


####setting (p,k)=(50,50)
n=100
pi1=0.5
p=50
sp=50
rho=0.8
tau=2.5
s0=1
s1=1
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	fullsim(n,pi1,p,beta,s0,s1,rho,tau,sp)
}
beta=0.5
for (s1 in c(0,0.2,0.4,0.6,0.8,1)){
	fullsim(n,pi1,p,beta,s1,s1,rho,tau,sp)
	fullsim(n,pi1,p,beta,0,s1,rho,tau,sp)
}
