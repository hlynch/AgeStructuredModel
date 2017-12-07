library(boot)
age<-c(3,4,5,6,7,8)
age.3<-c(0.02,0.01,0.04) #sd=0.01527525
age.4<-c(0.11,0.07,0.16) #sd=0.0450925
age.5<-c(0.37,0.16,0.47,0.49) #sd=0.1510794
age.6<-c(0.58,0.12,0.90,0.70) #sd=0.3308071
age.7<-c(0.50,0.75,0.86) #sd=0.1844813
age.8<-c(0.91,0.86) #sd=0.03535534
chicks<-c(mean(age.3),mean(age.4),mean(age.5),mean(age.6),mean(age.7),mean(age.8))
chicks<-chicks/2 #chicks per parent, therefore, fraction of max output

fit<-glm(chicks~age,family="binomial")
summary(fit)
plot(age,chicks,ylim=c(0,0.5),pch=15,col="gray")
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
lines(age,inv.logit(alpha0+beta0*age),col="gray",lwd=2)

#> alpha0
#[1] -4.844944
#> beta0
#[1] 0.6031969

# Hinke thesis - Table 3-1
# Reproductive success

age<-c(3,4,5,6,7) #7 = 7+
chicks<-c(0.28,0.36,0.48,0.52,0.91) #chicks per nest
chicks<-chicks/2 #chicks per parent, fraction of max output

fit<-glm(chicks~age,family="binomial")
summary(fit)
points(age,chicks,ylim=c(0,1.0),pch=15,col="gray")
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
lines(age,inv.logit(alpha0+beta0*age),col="gray",lwd=2)

alpha0.r1.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.r[1]",1]
alpha0.r2.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.r[2]",1]
alpha0.r3.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.r[3]",1]
alpha0.r4.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.r[4]",1]
alpha0.r5.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.r[5]",1]
beta0.r1.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.r[1]",1]
beta0.r2.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.r[2]",1]
beta0.r3.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.r[3]",1]
beta0.r4.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.r[4]",1]
beta0.r5.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.r[5]",1]


lines(age,inv.logit(alpha0.r1.est+beta0.r1.est*age),col=rgb(228,26,28,max=255),lwd=2)
lines(age,inv.logit(alpha0.r2.est+beta0.r2.est*age),col=rgb(55,126,184,max=255),lwd=2)
lines(age,inv.logit(alpha0.r3.est+beta0.r3.est*age),col=rgb(77,175,74,max=255),lwd=2)
lines(age,inv.logit(alpha0.r4.est+beta0.r4.est*age),col=rgb(152,78,163,max=255),lwd=2)
lines(age,inv.logit(alpha0.r5.est+beta0.r5.est*age),col=rgb(255,127,0,max=255),lwd=2)

########*********##########**********##########

age<-seq(2,8,1)
breeding.prop<-c(0,17,49,68,82,90,91)
breeding.prop<-breeding.prop/100
fit<-nls(breeding.prop~inv.logit(alpha0+beta0*age)*bSat,start=list(alpha0=-5.5,beta0=2.5,bSat=0.91))
plot(age,breeding.prop,ylim=c(0,1.0),pch=15,col="gray")
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
bSat<-summary(fit)$coefficients[3,1]
lines(age,inv.logit(alpha0+beta0*age)*bSat,col="gray",lwd=2)

age<-c(2,3,4,5,6,7) #7=7+
breeding.prop<-c(0,0.05,0.33,0.46,0.94,0.94)

# fit with bSat has bSat=1.04 so it was fixed at 1.0
fit<-nls(breeding.prop~inv.logit(alpha0+beta0*age),start=list(alpha0=-5.5,beta0=1.2))
points(age,breeding.prop,ylim=c(0,1.0),pch=15,col="gray")
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
lines(age,inv.logit(alpha0+beta0*age),col="gray",lwd=2)


alpha0.b1.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.b[1]",1]
alpha0.b2.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.b[2]",1]
alpha0.b3.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.b[3]",1]
alpha0.b4.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.b[4]",1]
alpha0.b5.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="alpha0.b[5]",1]
beta0.b1.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.b[1]",1]
beta0.b2.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.b[2]",1]
beta0.b3.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.b[3]",1]
beta0.b4.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.b[4]",1]
beta0.b5.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="beta0.b[5]",1]
#bSat1.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="bSat[1]",1]
#bSat2.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="bSat[2]",1]
#bSat3.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="bSat[3]",1]
#bSat4.est<-jagsfit.mcmc$summary[rownames(jagsfit.mcmc$summary)=="bSat[4]",1]

bSat1.est<-1
bSat2.est<-1
bSat3.est<-1
bSat4.est<-1
bSat5.est<-1

lines(age,inv.logit(alpha0.b1.est+beta0.b1.est*age)*bSat1.est,col=rgb(228,26,28,max=255),lwd=2)
lines(age,inv.logit(alpha0.b2.est+beta0.b2.est*age)*bSat2.est,col=rgb(55,126,184,max=255),lwd=2)
lines(age,inv.logit(alpha0.b3.est+beta0.b3.est*age)*bSat3.est,col=rgb(77,175,74,max=255),lwd=2)
lines(age,inv.logit(alpha0.b4.est+beta0.b4.est*age)*bSat4.est,col=rgb(152,78,163,max=255),lwd=2)
lines(age,inv.logit(alpha0.b5.est+beta0.b5.est*age)*bSat5.est,col=rgb(255,127,0,max=255),lwd=2)