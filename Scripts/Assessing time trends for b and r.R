nYears<-36
r1.region1.timeseries<-c()
for (i in 1:nYears)
{
  r1.region1.timeseries<-c(r1.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[1,",as.character(i),",1]",sep=""),1])
}

r1.region2.timeseries<-c()
for (i in 1:nYears)
{
  r1.region2.timeseries<-c(r1.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[2,",as.character(i),",1]",sep=""),1])
}

r1.region3.timeseries<-c()
for (i in 1:nYears)
{
  r1.region3.timeseries<-c(r1.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[3,",as.character(i),",1]",sep=""),1])
}

r1.region4.timeseries<-c()
for (i in 1:nYears)
{
  r1.region4.timeseries<-c(r1.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[4,",as.character(i),",1]",sep=""),1])
}

r1.region5.timeseries<-c()
for (i in 1:nYears)
{
  r1.region5.timeseries<-c(r1.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[5,",as.character(i),",1]",sep=""),1])
}


plot(seq(1979,2014),r1.region1.timeseries,typ="b",pch=16,ylim=c(0,1))
#lines(seq(1979,2014),r1.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),r1.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),r1.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),r1.region5.timeseries,typ="b",pch=16,col="orange")

###################################################################

r2.region1.timeseries<-c()
for (i in 1:nYears)
{
  r2.region1.timeseries<-c(r2.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[1,",as.character(i),",2]",sep=""),1])
}

r2.region2.timeseries<-c()
for (i in 1:nYears)
{
  r2.region2.timeseries<-c(r2.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[2,",as.character(i),",2]",sep=""),1])
}

r2.region3.timeseries<-c()
for (i in 1:nYears)
{
  r2.region3.timeseries<-c(r2.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[3,",as.character(i),",2]",sep=""),1])
}

r2.region4.timeseries<-c()
for (i in 1:nYears)
{
  r2.region4.timeseries<-c(r2.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[4,",as.character(i),",2]",sep=""),1])
}

r2.region5.timeseries<-c()
for (i in 1:nYears)
{
  r2.region5.timeseries<-c(r2.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[5,",as.character(i),",2]",sep=""),1])
}

plot(seq(1979,2014),r2.region1.timeseries,typ="b",pch=16,ylim=c(0,1))
#lines(seq(1979,2014),r2.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),r2.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),r2.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),r2.region5.timeseries,typ="b",pch=16,col="orange")

###################################################################

r3.region1.timeseries<-c()
for (i in 1:nYears)
{
  r3.region1.timeseries<-c(r3.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[1,",as.character(i),",3]",sep=""),1])
}

r3.region2.timeseries<-c()
for (i in 1:nYears)
{
  r3.region2.timeseries<-c(r3.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[2,",as.character(i),",3]",sep=""),1])
}

r3.region3.timeseries<-c()
for (i in 1:nYears)
{
  r3.region3.timeseries<-c(r3.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[3,",as.character(i),",3]",sep=""),1])
}

r3.region4.timeseries<-c()
for (i in 1:nYears)
{
  r3.region4.timeseries<-c(r3.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[4,",as.character(i),",3]",sep=""),1])
}

r3.region5.timeseries<-c()
for (i in 1:nYears)
{
  r3.region5.timeseries<-c(r3.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[5,",as.character(i),",3]",sep=""),1])
}

plot(seq(1979,2014),r3.region1.timeseries,typ="b",pch=16,ylim=c(min(r3.region1.timeseries,r3.region2.timeseries,r3.region3.timeseries,r3.region4.timeseries,r3.region1.timeseries),max(r3.region1.timeseries,r3.region2.timeseries,r3.region3.timeseries,r3.region4.timeseries,r3.region1.timeseries)))
#lines(seq(1979,2014),r3.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),r3.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),r3.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),r3.region5.timeseries,typ="b",pch=16,col="orange")
###################################################################

r4.region1.timeseries<-c()
for (i in 1:nYears)
{
  r4.region1.timeseries<-c(r4.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[1,",as.character(i),",4]",sep=""),1])
}

r4.region2.timeseries<-c()
for (i in 1:nYears)
{
  r4.region2.timeseries<-c(r4.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[2,",as.character(i),",4]",sep=""),1])
}

r4.region3.timeseries<-c()
for (i in 1:nYears)
{
  r4.region3.timeseries<-c(r4.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[3,",as.character(i),",4]",sep=""),1])
}

r4.region4.timeseries<-c()
for (i in 1:nYears)
{
  r4.region4.timeseries<-c(r4.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[4,",as.character(i),",4]",sep=""),1])
}

r4.region5.timeseries<-c()
for (i in 1:nYears)
{
  r4.region5.timeseries<-c(r4.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[5,",as.character(i),",4]",sep=""),1])
}

plot(seq(1979,2014),r4.region1.timeseries,typ="b",pch=16,ylim=c(0,1))
#lines(seq(1979,2014),r4.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),r4.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),r4.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),r4.region5.timeseries,typ="b",pch=16,col="orange")

###################################################################

r5.region1.timeseries<-c()
for (i in 1:nYears)
{
  r5.region1.timeseries<-c(r5.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[1,",as.character(i),",5]",sep=""),1])
}

r5.region2.timeseries<-c()
for (i in 1:nYears)
{
  r5.region2.timeseries<-c(r5.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[2,",as.character(i),",5]",sep=""),1])
}

r5.region3.timeseries<-c()
for (i in 1:nYears)
{
  r5.region3.timeseries<-c(r5.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[3,",as.character(i),",5]",sep=""),1])
}

r5.region4.timeseries<-c()
for (i in 1:nYears)
{
  r5.region4.timeseries<-c(r5.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[4,",as.character(i),",5]",sep=""),1])
}

r5.region5.timeseries<-c()
for (i in 1:nYears)
{
  r5.region5.timeseries<-c(r5.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("r[5,",as.character(i),",5]",sep=""),1])
}

plot(seq(1979,2014),r5.region1.timeseries,typ="b",pch=16,ylim=c(0,1))
#lines(seq(1979,2014),r5.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),r5.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),r5.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),r5.region5.timeseries,typ="b",pch=16,col="orange")

#############################

b1.region1.timeseries<-c()
for (i in 1:nYears)
{
  b1.region1.timeseries<-c(b1.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[1,",as.character(i),",1]",sep=""),1])
}

b1.region2.timeseries<-c()
for (i in 1:nYears)
{
  b1.region2.timeseries<-c(b1.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[2,",as.character(i),",1]",sep=""),1])
}

b1.region3.timeseries<-c()
for (i in 1:nYears)
{
  b1.region3.timeseries<-c(b1.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[3,",as.character(i),",1]",sep=""),1])
}

b1.region4.timeseries<-c()
for (i in 1:nYears)
{
  b1.region4.timeseries<-c(b1.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[4,",as.character(i),",1]",sep=""),1])
}

b1.region5.timeseries<-c()
for (i in 1:nYears)
{
  b1.region5.timeseries<-c(b1.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[5,",as.character(i),",1]",sep=""),1])
}

plot(seq(1979,2014),b1.region1.timeseries,typ="b",pch=16,ylim=c(min(b1.region1.timeseries,b1.region2.timeseries,b1.region3.timeseries,b1.region4.timeseries,b1.region1.timeseries),max(b1.region1.timeseries,b1.region2.timeseries,b1.region3.timeseries,b1.region4.timeseries,b1.region1.timeseries)))
#lines(seq(1979,2014),b1.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),b1.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),b1.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),b1.region5.timeseries,typ="b",pch=16,col="orange")

###################################################################

b2.region1.timeseries<-c()
for (i in 1:nYears)
{
  b2.region1.timeseries<-c(b2.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[1,",as.character(i),",2]",sep=""),1])
}

b2.region2.timeseries<-c()
for (i in 1:nYears)
{
  b2.region2.timeseries<-c(b2.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[2,",as.character(i),",2]",sep=""),1])
}

b2.region3.timeseries<-c()
for (i in 1:nYears)
{
  b2.region3.timeseries<-c(b2.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[3,",as.character(i),",2]",sep=""),1])
}

b2.region4.timeseries<-c()
for (i in 1:nYears)
{
  b2.region4.timeseries<-c(b2.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[4,",as.character(i),",2]",sep=""),1])
}

b2.region5.timeseries<-c()
for (i in 1:nYears)
{
  b2.region5.timeseries<-c(b2.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[5,",as.character(i),",2]",sep=""),1])
}

plot(seq(1979,2014),b2.region1.timeseries,typ="b",pch=16,ylim=c(min(b2.region1.timeseries,b2.region2.timeseries,b2.region3.timeseries,b2.region4.timeseries,b2.region1.timeseries),max(b2.region1.timeseries,b2.region2.timeseries,b2.region3.timeseries,b2.region4.timeseries,b2.region1.timeseries)))
#lines(seq(1979,2014),b2.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),b2.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),b2.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),b2.region5.timeseries,typ="b",pch=16,col="orange")

###################################################################

b3.region1.timeseries<-c()
for (i in 1:nYears)
{
  b3.region1.timeseries<-c(b3.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[1,",as.character(i),",3]",sep=""),1])
}

b3.region2.timeseries<-c()
for (i in 1:nYears)
{
  b3.region2.timeseries<-c(b3.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[2,",as.character(i),",3]",sep=""),1])
}

b3.region3.timeseries<-c()
for (i in 1:nYears)
{
  b3.region3.timeseries<-c(b3.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[3,",as.character(i),",3]",sep=""),1])
}

b3.region4.timeseries<-c()
for (i in 1:nYears)
{
  b3.region4.timeseries<-c(b3.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[4,",as.character(i),",3]",sep=""),1])
}

b3.region5.timeseries<-c()
for (i in 1:nYears)
{
  b3.region5.timeseries<-c(b3.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[5,",as.character(i),",3]",sep=""),1])
}

plot(seq(1979,2014),b3.region1.timeseries,typ="b",pch=16,ylim=c(min(b3.region1.timeseries,b3.region2.timeseries,b3.region3.timeseries,b3.region4.timeseries,b3.region1.timeseries),max(b3.region1.timeseries,b3.region2.timeseries,b3.region3.timeseries,b3.region4.timeseries,b3.region1.timeseries)))
#lines(seq(1979,2014),b3.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),b3.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),b3.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),b3.region5.timeseries,typ="b",pch=16,col="orange")

###################################################################

b4.region1.timeseries<-c()
for (i in 1:nYears)
{
  b4.region1.timeseries<-c(b4.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[1,",as.character(i),",4]",sep=""),1])
}

b4.region2.timeseries<-c()
for (i in 1:nYears)
{
  b4.region2.timeseries<-c(b4.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[2,",as.character(i),",4]",sep=""),1])
}

b4.region3.timeseries<-c()
for (i in 1:nYears)
{
  b4.region3.timeseries<-c(b4.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[3,",as.character(i),",4]",sep=""),1])
}

b4.region4.timeseries<-c()
for (i in 1:nYears)
{
  b4.region4.timeseries<-c(b4.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[4,",as.character(i),",4]",sep=""),1])
}

b4.region5.timeseries<-c()
for (i in 1:nYears)
{
  b4.region5.timeseries<-c(b4.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[5,",as.character(i),",4]",sep=""),1])
}

plot(seq(1979,2014),b4.region1.timeseries,typ="b",pch=16,ylim=c(min(b4.region1.timeseries,b4.region2.timeseries,b4.region3.timeseries,b4.region4.timeseries,b4.region1.timeseries),max(b4.region1.timeseries,b4.region2.timeseries,b4.region3.timeseries,b4.region4.timeseries,b4.region1.timeseries)))
#lines(seq(1979,2014),b4.region2.timeseries,typ="b",pch=16,col="red")
lines(seq(1979,2014),b4.region3.timeseries,typ="b",pch=16,col="blue")
#lines(seq(1979,2014),b4.region4.timeseries,typ="b",pch=16,col="green")
lines(seq(1979,2014),b4.region5.timeseries,typ="b",pch=16,col="orange")

##################################

b5.region1.timeseries<-c()
for (i in 1:nYears)
{
  b5.region1.timeseries<-c(b5.region1.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[1,",as.character(i),",5]",sep=""),1])
}

b5.region2.timeseries<-c()
for (i in 1:nYears)
{
  b5.region2.timeseries<-c(b5.region2.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[2,",as.character(i),",5]",sep=""),1])
}

b5.region3.timeseries<-c()
for (i in 1:nYears)
{
  b5.region3.timeseries<-c(b5.region3.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[3,",as.character(i),",5]",sep=""),1])
}

b5.region4.timeseries<-c()
for (i in 1:nYears)
{
  b5.region4.timeseries<-c(b5.region4.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[4,",as.character(i),",5]",sep=""),1])
}

b5.region5.timeseries<-c()
for (i in 1:nYears)
{
  b5.region5.timeseries<-c(b5.region5.timeseries,jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)==paste("b[5,",as.character(i),",5]",sep=""),1])
}

plot(seq(1979,2014),b5.region1.timeseries,typ="b",pch=16,ylim=c(min(b5.region1.timeseries,b5.region2.timeseries,b5.region3.timeseries,b5.region4.timeseries,b5.region1.timeseries),max(b5.region1.timeseries,b5.region2.timeseries,b5.region3.timeseries,b5.region4.timeseries,b5.region1.timeseries)),col=rgb(228,26,28,max=255),lwd=2)
#lines(seq(1979,2014),b5.region2.timeseries,typ="b",pch=16,col=rgb(55,126,184,max=255),lwd=2)
lines(seq(1979,2014),b5.region3.timeseries,typ="b",pch=16,col=rgb(77,175,74,max=255),lwd=2)
#lines(seq(1979,2014),b5.region4.timeseries,typ="b",pch=16,col=rgb(152,78,163,max=255),lwd=2)
lines(seq(1979,2014),b5.region5.timeseries,typ="b",pch=16,col=rgb(255,127,0,max=255),lwd=2)
