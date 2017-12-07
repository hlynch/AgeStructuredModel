####Region 1

alpha0.r1.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="alpha0.r[1]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="alpha0.r[1]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="alpha0.r[1]"]))

beta0.r1.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="beta0.r[1]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="beta0.r[1]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="beta0.r[1]"]))

r.age3.est<-inv.logit(alpha0.r1.est+beta0.r1.est*3)
r.age4.est<-inv.logit(alpha0.r1.est+beta0.r1.est*4)
r.age5.est<-inv.logit(alpha0.r1.est+beta0.r1.est*5)
r.age6.est<-inv.logit(alpha0.r1.est+beta0.r1.est*6)
r.age7.est<-inv.logit(alpha0.r1.est+beta0.r1.est*7)
r.age3.est*2 #chicks per nest
r.age4.est*2
r.age5.est*2
r.age6.est*2
r.age7.est*2

alpha0.b1.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="alpha0.b[1]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="alpha0.b[1]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="alpha0.b[1]"]))

beta0.b1.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="beta0.b[1]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="beta0.b[1]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="beta0.b[1]"]))

b.age3.est<-inv.logit(alpha0.b1.est+beta0.b1.est*3)
b.age4.est<-inv.logit(alpha0.b1.est+beta0.b1.est*4)
b.age5.est<-inv.logit(alpha0.b1.est+beta0.b1.est*5)
b.age6.est<-inv.logit(alpha0.b1.est+beta0.b1.est*6)
b.age7.est<-inv.logit(alpha0.b1.est+beta0.b1.est*7)
b.age3.est
b.age4.est
b.age5.est
b.age6.est
b.age7.est


omega.juv.intercept.region1.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region1"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region1"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region1"]))
inv.logit(omega.juv.intercept.region1.est)

omega.ad.intercept.region1.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region1"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region1"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region1"]))
inv.logit(omega.ad.intercept.region1.est)




##Region 3

alpha0.r3.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="alpha0.r[3]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="alpha0.r[3]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="alpha0.r[3]"]))

beta0.r3.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="beta0.r[3]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="beta0.r[3]"],jagsfit.mcmc$sims.array[1,3,rownames(jagsfit.mcmc$summary)=="beta0.r[3]"]))

r.age3.est<-inv.logit(alpha0.r3.est+beta0.r3.est*3)
r.age4.est<-inv.logit(alpha0.r3.est+beta0.r3.est*4)
r.age5.est<-inv.logit(alpha0.r3.est+beta0.r3.est*5)
r.age6.est<-inv.logit(alpha0.r3.est+beta0.r3.est*6)
r.age7.est<-inv.logit(alpha0.r3.est+beta0.r3.est*7)
r.age3.est*2 #chicks per nest
r.age4.est*2
r.age5.est*2
r.age6.est*2
r.age7.est*2

alpha0.b3.est<-mean(c(jagsfit.mcmc$sims.array[1,1,rownames(jagsfit.mcmc$summary)=="alpha0.b[3]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="alpha0.b[3]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="alpha0.b[3]"]))

beta0.b3.est<-mean(c(jagsfit.mcmc$sims.array[1,1,rownames(jagsfit.mcmc$summary)=="beta0.b[3]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="beta0.b[3]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="beta0.b[3]"]))

b.age3.est<-inv.logit(alpha0.b3.est+beta0.b3.est*3)
b.age4.est<-inv.logit(alpha0.b3.est+beta0.b3.est*4)
b.age5.est<-inv.logit(alpha0.b3.est+beta0.b3.est*5)
b.age6.est<-inv.logit(alpha0.b3.est+beta0.b3.est*6)
b.age7.est<-inv.logit(alpha0.b3.est+beta0.b3.est*7)
b.age3.est
b.age4.est
b.age5.est
b.age6.est
b.age7.est


omega.juv.intercept.region3.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region3"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region3"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region3"]))
inv.logit(omega.juv.intercept.region3.est)

omega.ad.intercept.region3.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region3"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region3"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region3"]))
inv.logit(omega.ad.intercept.region3.est)


##Region 5

alpha0.r5.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="alpha0.r[5]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="alpha0.r[5]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="alpha0.r[5]"]))

beta0.r5.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="beta0.r[5]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="beta0.r[5]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="beta0.r[5]"]))

r.age3.est<-inv.logit(alpha0.r5.est+beta0.r5.est*3)
r.age4.est<-inv.logit(alpha0.r5.est+beta0.r5.est*4)
r.age5.est<-inv.logit(alpha0.r5.est+beta0.r5.est*5)
r.age6.est<-inv.logit(alpha0.r5.est+beta0.r5.est*6)
r.age7.est<-inv.logit(alpha0.r5.est+beta0.r5.est*7)
r.age3.est*2 #chicks per nest
r.age4.est*2
r.age5.est*2
r.age6.est*2
r.age7.est*2

alpha0.b5.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="alpha0.b[5]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="alpha0.b[5]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="alpha0.b[5]"]))

beta0.b5.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="beta0.b[5]"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="beta0.b[5]"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="beta0.b[5]"]))

b.age3.est<-inv.logit(alpha0.b5.est+beta0.b5.est*3)
b.age4.est<-inv.logit(alpha0.b5.est+beta0.b5.est*4)
b.age5.est<-inv.logit(alpha0.b5.est+beta0.b5.est*5)
b.age6.est<-inv.logit(alpha0.b5.est+beta0.b5.est*6)
b.age7.est<-inv.logit(alpha0.b5.est+beta0.b5.est*7)
b.age3.est
b.age4.est
b.age5.est
b.age6.est
b.age7.est


omega.juv.intercept.region5.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region5"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region5"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region5"]))
inv.logit(omega.juv.intercept.region5.est)

omega.ad.intercept.region5.est<-mean(c(jagsfit.mcmc$sims.array[,1,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region5"],jagsfit.mcmc$sims.array[,2,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region5"],jagsfit.mcmc$sims.array[,3,rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region5"]))
inv.logit(omega.ad.intercept.region5.est)


phi.ad<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="phi.ad",1]
phi.ad

phi.juv<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="phi.juv",1]
phi.juv

phi.b<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="phi.b",1]
phi.b

phi.r<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="phi.r",1]
phi.r
