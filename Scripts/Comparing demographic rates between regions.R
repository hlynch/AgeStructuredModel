####Region 1

alpha0.r1.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="alpha0.r[1]")]
beta0.r1.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="beta0.r[1]")]
r.age3.est.r1<-inv.logit(alpha0.r1.est+beta0.r1.est*3)
r.age4.est.r1<-inv.logit(alpha0.r1.est+beta0.r1.est*4)
r.age5.est.r1<-inv.logit(alpha0.r1.est+beta0.r1.est*5)
r.age6.est.r1<-inv.logit(alpha0.r1.est+beta0.r1.est*6)
r.age7.est.r1<-inv.logit(alpha0.r1.est+beta0.r1.est*7)
#hist(r.age3.est*2,breaks=30) #chicks per nest


alpha0.b1.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="alpha0.b[1]")]
beta0.b1.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="beta0.b[1]")]
bSat.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="bSat[1]")]
b.age3.est.r1<-inv.logit(alpha0.b1.est+beta0.b1.est*3)
b.age4.est.r1<-inv.logit(alpha0.b1.est+beta0.b1.est*4)
b.age5.est.r1<-inv.logit(alpha0.b1.est+beta0.b1.est*5)
b.age6.est.r1<-inv.logit(alpha0.b1.est+beta0.b1.est*6)
b.age7.est.r1<-inv.logit(alpha0.b1.est+beta0.b1.est*7)

omega.juv.intercept.region1.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="omega.juv.intercept.region1")]
omega.juv.intercept.region1.est<-inv.logit(omega.juv.intercept.region1.est)

omega.ad.intercept.region1.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="omega.ad.intercept.region1")]
omega.ad.intercept.region1.est<-inv.logit(omega.ad.intercept.region1.est)

####Region 3

alpha0.r3.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="alpha0.r[3]")]
beta0.r3.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="beta0.r[3]")]
r.age3.est.r3<-inv.logit(alpha0.r3.est+beta0.r3.est*3)
r.age4.est.r3<-inv.logit(alpha0.r3.est+beta0.r3.est*4)
r.age5.est.r3<-inv.logit(alpha0.r3.est+beta0.r3.est*5)
r.age6.est.r3<-inv.logit(alpha0.r3.est+beta0.r3.est*6)
r.age7.est.r3<-inv.logit(alpha0.r3.est+beta0.r3.est*7)
#hist(r.age3.est*2,breaks=30) #chicks per nest


alpha0.b3.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="alpha0.b[3]")]
beta0.b3.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="beta0.b[3]")]
b.age3.est.r3<-inv.logit(alpha0.b3.est+beta0.b3.est*3)
b.age4.est.r3<-inv.logit(alpha0.b3.est+beta0.b3.est*4)
b.age5.est.r3<-inv.logit(alpha0.b3.est+beta0.b3.est*5)
b.age6.est.r3<-inv.logit(alpha0.b3.est+beta0.b3.est*6)
b.age7.est.r3<-inv.logit(alpha0.b3.est+beta0.b3.est*7)

omega.juv.intercept.region3.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="omega.juv.intercept.region3")]
omega.juv.intercept.region3.est<-inv.logit(omega.juv.intercept.region3.est)

omega.ad.intercept.region3.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="omega.ad.intercept.region3")]
omega.ad.intercept.region3.est<-inv.logit(omega.ad.intercept.region3.est)


####Region 5

alpha0.r5.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="alpha0.r[5]")]
beta0.r5.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="beta0.r[5]")]
r.age3.est.r5<-inv.logit(alpha0.r5.est+beta0.r5.est*3)
r.age4.est.r5<-inv.logit(alpha0.r5.est+beta0.r5.est*4)
r.age5.est.r5<-inv.logit(alpha0.r5.est+beta0.r5.est*5)
r.age6.est.r5<-inv.logit(alpha0.r5.est+beta0.r5.est*6)
r.age7.est.r5<-inv.logit(alpha0.r5.est+beta0.r5.est*7)
#hist(r.age3.est*2,breaks=30) #chicks per nest


alpha0.b5.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="alpha0.b[5]")]
beta0.b5.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="beta0.b[5]")]
b.age3.est.r5<-inv.logit(alpha0.b5.est+beta0.b5.est*3)
b.age4.est.r5<-inv.logit(alpha0.b5.est+beta0.b5.est*4)
b.age5.est.r5<-inv.logit(alpha0.b5.est+beta0.b5.est*5)
b.age6.est.r5<-inv.logit(alpha0.b5.est+beta0.b5.est*6)
b.age7.est.r5<-inv.logit(alpha0.b5.est+beta0.b5.est*7)

omega.juv.intercept.region5.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="omega.juv.intercept.region5")]
omega.juv.intercept.region5.est<-inv.logit(omega.juv.intercept.region5.est)

omega.ad.intercept.region5.est<-jagsfit.mcmc$sims.matrix[,which(colnames(jagsfit.mcmc$sims.matrix)=="omega.ad.intercept.region5")]
omega.ad.intercept.region5.est<-inv.logit(omega.ad.intercept.region5.est)



############


a<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region1")==T)]
b<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region5")==T)]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))

a<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region1")==T)]
b<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region5")==T)]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))

a<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="beta0.r[1]")==T)]
b<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="beta0.r[5]")==T)]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))

a<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="alpha0.r[1]")==T)]
b<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="alpha0.r[5]")==T)]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))

a<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="alpha0.b[1]")==T)]
b<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="alpha0.b[5]")==T)]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))

a<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="beta0.b[1]")==T)]
b<-jagsfit.mcmc$sims.matrix[,which((rownames(jagsfit.mcmc$summary)=="beta0.b[5]")==T)]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))

