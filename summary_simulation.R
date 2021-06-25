####Figure 1
n=100
pi1=0.5
p=50
rho=0.8
tau=2.5
s0=1
s1=1

pdf("plot1_1.pdf")
par(mfrow=c(1,2))
sp=0
res=NULL
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,8])
}
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
dev.off()



pdf("plot1_2.pdf")
par(mfrow=c(1,2))
sp=10
res=NULL
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,8])
}
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
dev.off()


pdf("plot1_3.pdf")
par(mfrow=c(1,2))
sp=50
res=NULL
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,8])
}
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
dev.off()



####Figure 2
n=100
pi1=0.5
p=50
rho=0.8
tau=2.5
beta=0.5

pdf("plot2_1.pdf")
par(mfrow=c(1,2))
sp=10
res=NULL
for (s0 in c(0,0.2,0.4,0.6,0.8,1)){
	s1=s0
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,5])
}
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
dev.off()


pdf("plot2_2.pdf")
par(mfrow=c(1,2))
sp=50
res=NULL
for (s0 in c(0,0.2,0.4,0.6,0.8,1)){
	s1=s0
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,5])
}
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
dev.off()



pdf("plot2_3.pdf")
par(mfrow=c(1,2))
sp=50
res=NULL
for (s1 in c(0,0.2,0.4,0.6,0.8,1)){
	s0=0
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,5])
}
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
dev.off()





####Figure S1
n=100
pi1=0.5
p=10
rho=0.8
tau=2.5
sp=10

pdf("plotS1_1.pdf")
par(mfrow=c(3,1))
s0=0
s1=0
res=NULL
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,8])
}
plot(res[1,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="Cox")
lines(res[2,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[3,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[4,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
}
dev.off()



pdf("plotS1_2.pdf")
par(mfrow=c(3,1))
s0=1
s1=1
res=NULL
for (beta in c(0,0.2,0.4,0.6,0.8,1)){
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,8])
}
plot(res[1,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="Cox")
lines(res[2,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[3,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[4,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="Power",xlab=expression(beta),ylim=c(0,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
}
dev.off()






####Figure S2
n=100
pi1=0.5
p=10
rho=0.8
tau=2.5
beta=10
sp=10

pdf("plotS2_1.pdf")
par(mfrow=c(3,1))
res=NULL
for (s1 in c(0,0.2,0.4,0.6,0.8,1)){
	s0=s1
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,5])
}
plot(res[1,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="Cox")
lines(res[2,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[3,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[4,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
}
dev.off()



pdf("plotS2_2.pdf")
par(mfrow=c(3,1))
s0=0
res=NULL
for (s1 in c(0,0.2,0.4,0.6,0.8,1)){
	filename<-paste("Summary-simple-n=",as.character(n),"pi=",as.character(pi1),"p=",as.character(p),"beta=",as.character(beta),"s0=",as.character(s0),"s1=",as.character(s1),"rho=",as.character(rho),"tau=",as.character(tau),"sp=",as.character(sp),".csv",sep="")
	data<-read.csv(filename)
	res<-cbind(res,data[,5])
}
plot(res[1,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="Cox")
lines(res[2,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[3,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[4,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[5,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="LASSO")
lines(res[6,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[7,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[8,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
plot(res[9,]~c(0,0.2,0.4,0.6,0.8,1),type="l",ylab="RelMSE",xlab=expression(s[1]),ylim=c(0.4,1.2),main="RF")
lines(res[10,]~c(0,0.2,0.4,0.6,0.8,1),col="red")
lines(res[11,]~c(0,0.2,0.4,0.6,0.8,1),col="blue")
lines(res[12,]~c(0,0.2,0.4,0.6,0.8,1),col="green")
legend("bottomleft",c(expression(hat(tau)[0]),expression(hat(tau)[1]),expression(hat(tau)[2]),expression(hat(tau)[3])),col=c("black","red","blue","green"),lty=1,cex=0.4)
}
dev.off()





