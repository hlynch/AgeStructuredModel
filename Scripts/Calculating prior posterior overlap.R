#Calculate the priors
library(ggplot2)

alpha0.r<-list()
alpha0.b<-list()
beta0.r<-list()
beta0.b<-list()
bSat<-list()

alpha0.r[[1]]<-rnorm(5000,-3.97,1.23) # mean=-3.97,sd=1.23
alpha0.r[[2]]<-rnorm(5000,-3.97,1.23) # mean=-3.97,sd=1.23
alpha0.r[[3]]<-rnorm(5000,-3.97,1.23) # mean=-3.97,sd=1.23
alpha0.r[[4]]<-rnorm(5000,-3.97,1.23) # mean=-3.97,sd=1.23
alpha0.r[[5]]<-rnorm(5000,-3.97,1.23) # mean=-3.97,sd=1.23

beta0.r[[1]]<-rnorm(5000,0.496,0.15) # mean=0.496, sd=0.15
beta0.r[[2]]<-rnorm(5000,0.496,0.15) # mean=0.496, sd=0.15
beta0.r[[3]]<-rnorm(5000,0.496,0.15) # mean=0.496, sd=0.15
beta0.r[[4]]<-rnorm(5000,0.496,0.15) # mean=0.496, sd=0.15
beta0.r[[5]]<-rnorm(5000,0.496,0.15) # mean=0.496, sd=0.15

#bSat[[1]]<-rbeta(1000,7.75,sqrt(1/0.42)) #using commented code above
#bSat[[2]]<-rbeta(1000,7.75,sqrt(1/0.42)) #using commented code above
#bSat[[3]]<-rbeta(1000,7.75,sqrt(1/0.42)) #using commented code above
#bSat[[4]]<-rbeta(1000,7.75,sqrt(1/0.42)) #using commented code above

alpha0.b[[1]]<-rnorm(5000,-6.14,0.86) #mean=-6.14, sd=0.86
alpha0.b[[2]]<-rnorm(5000,-6.14,0.86) #mean=-6.14, sd=0.86
alpha0.b[[3]]<-rnorm(5000,-6.14,0.86) #mean=-6.14, sd=0.86
alpha0.b[[4]]<-rnorm(5000,-6.14,0.86) #mean=-6.14, sd=0.86
alpha0.b[[5]]<-rnorm(5000,-6.14,0.86) #mean=-6.14, sd=0.86

beta0.b[[1]]<-rnorm(5000,1.391,0.017) #mean=1.391, sd=0.017
beta0.b[[2]]<-rnorm(5000,1.391,0.017) #mean=1.391, sd=0.017
beta0.b[[3]]<-rnorm(5000,1.391,0.017) #mean=1.391, sd=0.017
beta0.b[[4]]<-rnorm(5000,1.391,0.017) #mean=1.391, sd=0.017
beta0.b[[5]]<-rnorm(5000,1.391,0.017) #mean=1.391, sd=0.017

# change prior on omega to match data we have from Hinke and Ainley

#temp<-rgamma(10000,1,1)
#phi.b <- sample(temp[temp>0.5],size=1000,replace=F)
phi.b <- rgamma(5000,1,1)
phi.r <- rgamma(5000,1,1)
phi.ad <- rgamma(5000,1,1)
phi.juv <- rgamma(5000,1,1)

omega.juv.intercept.region1.prior<-rnorm(5000,0.005, sqrt(1/6)) #logit(0.50125)=0.005
omega.ad.intercept.region1.prior<-rnorm(5000, 1.67, sqrt(1/4))   #logit(0.842)=1.67
omega.juv.intercept.region2.prior<-rnorm(5000,0.005, sqrt(1/6)) #precision based on personal
omega.ad.intercept.region2.prior<-rnorm(5000,1.67, sqrt(1/4))   #estimation of how far off the
omega.juv.intercept.region3.prior<-rnorm(5000,0.005, sqrt(1/6)) #mean surv from banding could
omega.ad.intercept.region3.prior<-rnorm(5000,1.67, sqrt(1/4))   #possibly be
omega.juv.intercept.region4.prior<-rnorm(5000,0.005, sqrt(1/6))
omega.ad.intercept.region4.prior<-rnorm(5000,1.67, sqrt(1/4))
omega.juv.intercept.region5.prior<-rnorm(5000,0.005, sqrt(1/6))
omega.ad.intercept.region5.prior<-rnorm(5000,1.67, sqrt(1/4))


plot.function=function(prior.vec,posterior.vec,overlap)
{
  prior <- data.frame(values=prior.vec)
  posterior <- data.frame(values=posterior.vec)
  prior$name <- 'prior'
  posterior$name <- 'posterior'
  Results <- rbind(prior, posterior)
  binsize<-(max(Results$values)-min(Results$values))/50
  ggplot(Results, aes(values, fill = name)) + geom_histogram(alpha = 0.5, binwidth=binsize, aes(y = ..density..), position = 'identity') + ggtitle(paste(as.character(round(overlap,digits=2)),"% overlap",sep=""))
}

calc.overlap=function(prior.vec,posterior.vec)
{
  prior <- data.frame(values=prior.vec)
  posterior <- data.frame(values=posterior.vec)
  prior$name <- 'prior'
  posterior$name <- 'posterior'
  Results <- rbind(prior, posterior)
  binsize<-(max(Results$values)-min(Results$values))/50
  s<-seq(min(Results$values),max(Results$values),by=binsize)
  overlap<-vector(length=length(s))
  for (i in 1:(length(s)-1))
    {
      prior.num<-sum(as.numeric((prior.vec>s[i])&(prior.vec<s[i+1])))/length(prior.vec)
      posterior.num<-sum(as.numeric((posterior.vec>s[i])&(posterior.vec<s[i+1])))/length(posterior.vec)
      overlap[i]<-min(prior.num,posterior.num)
    }
  percent.overlap=sum(overlap)
  return(percent.overlap)
}

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.b[1]")==T)
a<-alpha0.b[[1]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.b1.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.b[2]")==T)
a<-alpha0.b[[2]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.b2.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.b[3]")==T)
a<-alpha0.b[[3]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.b3.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.b[4]")==T)
a<-alpha0.b[[4]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.b4.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.b[5]")==T)
a<-alpha0.b[[5]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.b5.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.b[1]")==T)
a<-beta0.b[[1]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.b1.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.b[2]")==T)
a<-beta0.b[[2]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.b2.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.b[3]")==T)
a<-beta0.b[[3]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.b3.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.b[4]")==T)
a<-beta0.b[[4]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.b4.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.b[5]")==T)
a<-beta0.b[[5]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.b5.png")
dev.off()

#temp<-which((rownames(jagsfit.mcmc$summary)=="bSat[1]")==T)
#a<-bSat[[1]]
#b<-jagsfit.mcmc$sims.matrix[,temp]
#plot.function(a,b,overlap=(calc.overlap(a,b)*100))
#mean(jagsfit.mcmc$sims.matrix[,temp])
#dev.copy(png,"Results//FinalESARun//Prior posterior overlap//bSat1.png")
#dev.off()


temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.r[1]")==T)
a<-alpha0.r[[1]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.r1.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.r[2]")==T)
a<-alpha0.r[[2]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.r2.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.r[3]")==T)
a<-alpha0.r[[3]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.r3.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.r[4]")==T)
a<-alpha0.r[[4]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.r4.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="alpha0.r[5]")==T)
a<-alpha0.r[[5]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//alpha0.r5.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.r[1]")==T)
a<-beta0.r[[1]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.r1.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.r[2]")==T)
a<-beta0.r[[2]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.r2.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.r[3]")==T)
a<-beta0.r[[3]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.r3.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.r[4]")==T)
a<-beta0.r[[4]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.r4.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="beta0.r[5]")==T)
a<-beta0.r[[5]]
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//beta0.r5.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region1")==T)
a<-omega.ad.intercept.region1.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
mean(jagsfit.mcmc$sims.matrix[,temp])
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.ad.intercept.region1.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region2")==T)
a<-omega.ad.intercept.region2.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
mean(jagsfit.mcmc$sims.matrix[,temp])
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.ad.intercept.region2.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region3")==T)
a<-omega.ad.intercept.region3.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
mean(jagsfit.mcmc$sims.matrix[,temp])
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.ad.intercept.region3.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region4")==T)
a<-omega.ad.intercept.region4.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
mean(jagsfit.mcmc$sims.matrix[,temp])
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.ad.intercept.region4.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.ad.intercept.region5")==T)
a<-omega.ad.intercept.region5.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
mean(jagsfit.mcmc$sims.matrix[,temp])
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.ad.intercept.region5.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region1")==T)
a<-omega.juv.intercept.region1.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.juv.intercept.region1.png")
dev.off()
mean(jagsfit.mcmc$sims.matrix[,temp])

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region2")==T)
a<-omega.juv.intercept.region2.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.juv.intercept.region2.png")
dev.off()
mean(jagsfit.mcmc$sims.matrix[,temp])

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region3")==T)
a<-omega.juv.intercept.region3.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.juv.intercept.region3.png")
dev.off()
mean(jagsfit.mcmc$sims.matrix[,temp])

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region4")==T)
a<-omega.juv.intercept.region4.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.juv.intercept.region4.png")
dev.off()
mean(jagsfit.mcmc$sims.matrix[,temp])

temp<-which((rownames(jagsfit.mcmc$summary)=="omega.juv.intercept.region5")==T)
a<-omega.juv.intercept.region5.prior
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//omega.juv.intercept.region5.png")
dev.off()
mean(jagsfit.mcmc$sims.matrix[,temp])

temp<-which((rownames(jagsfit.mcmc$summary)=="phi.juv")==T)
a<-phi.juv
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//phi.juv.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="phi.ad")==T)
a<-phi.ad
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//phi.ad.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="phi.b")==T)
a<-phi.b
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//phi.b.png")
dev.off()

temp<-which((rownames(jagsfit.mcmc$summary)=="phi.r")==T)
a<-phi.r
b<-jagsfit.mcmc$sims.matrix[,temp]
plot.function(a,b,overlap=(calc.overlap(a,b)*100))
dev.copy(png,"Results//FinalESARun//Prior posterior overlap//phi.r.png")
dev.off()
