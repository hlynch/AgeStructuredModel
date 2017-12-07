################################
#
# July 23, 2015
# Added CROZ data
#
# July 24, 2015
# Fixed the prior for the interannual variation to make a inv-gamma
# instead of a normal
#
# Aug 3, 2015
# Changed the way the r and b are modelled, now use beta regression
#
# Sept 30, 2015
# I don't understand why I bother defining a prior for each region for juv omega is I only use region1
#
################################

#setwd("/Volumes/LYNCH USB/Adelie state space modelling/state space 2015/")
setwd("~/Desktop/Adelie state space modelling/State space 2015/")
sink("Adelie simplified life cycle v10.jags")
cat("
    model {

    # Specify the priors for all parameters in the model

    alpha0.r[1]~dnorm(-3.97,0.66) # mean=-3.97,sd=1.23
    alpha0.r[2]~dnorm(-3.97,0.66) # mean=-3.97,sd=1.23
    alpha0.r[3]~dnorm(-3.97,0.66) # mean=-3.97,sd=1.23
    alpha0.r[4]~dnorm(-3.97,0.66) # mean=-3.97,sd=1.23
    alpha0.r[5]~dnorm(-3.97,0.66) # mean=-3.97,sd=1.23

    beta0.r[1]~dnorm(0.496,44)I(0,) # mean=0.496, sd=0.15
    beta0.r[2]~dnorm(0.496,44)I(0,) # mean=0.496, sd=0.15
    beta0.r[3]~dnorm(0.496,44)I(0,) # mean=0.496, sd=0.15
    beta0.r[4]~dnorm(0.496,44)I(0,) # mean=0.496, sd=0.15
    beta0.r[5]~dnorm(0.496,44)I(0,) # mean=0.496, sd=0.15

    alpha0.b[1]~dnorm(-6.14,1.35) #mean=-6.14, sd=0.86
    alpha0.b[2]~dnorm(-6.14,1.35) #mean=-6.14, sd=0.86
    alpha0.b[3]~dnorm(-6.14,1.35) #mean=-6.14, sd=0.86
    alpha0.b[4]~dnorm(-6.14,1.35) #mean=-6.14, sd=0.86
    alpha0.b[5]~dnorm(-6.14,1.35) #mean=-6.14, sd=0.86

    beta0.b[1]~dnorm(1.391,3460)I(0,) #mean=1.391, sd=0.017
    beta0.b[2]~dnorm(1.391,3460)I(0,) #mean=1.391, sd=0.017
    beta0.b[3]~dnorm(1.391,3460)I(0,) #mean=1.391, sd=0.017
    beta0.b[4]~dnorm(1.391,3460)I(0,) #mean=1.391, sd=0.017
    beta0.b[5]~dnorm(1.391,3460)I(0,) #mean=1.391, sd=0.017


# change prior on omega to match data we have from Hinke and Ainley
# original precision based on personal estimation of how far off mean survival from banding might be

    omega.juv.intercept.region1 ~ dnorm(0.005, 6) #logit(0.50125)=0.005
    omega.ad.intercept.region1 ~ dnorm(1.67, 4)   #logit(0.842)=1.67
    omega.juv.intercept.region2 ~ dnorm(0.005, 6) #precision based on personal
    omega.ad.intercept.region2 ~ dnorm(1.67, 4)   #estimation of how far off the
    omega.juv.intercept.region3 ~ dnorm(0.005, 6) #mean surv from banding could
    omega.ad.intercept.region3 ~ dnorm(1.67, 4)   #possibly be
    omega.juv.intercept.region4 ~ dnorm(0.005, 6)
    omega.ad.intercept.region4 ~ dnorm(1.67, 4)
    omega.juv.intercept.region5 ~ dnorm(0.005, 6)
    omega.ad.intercept.region5 ~ dnorm(1.67, 4)

    phi.b ~ dgamma(1,1)
    phi.r ~ dgamma(1,1)
    phi.ad ~ dgamma(1,1)
    phi.juv ~ dgamma(1,1)

    for (k in 1:nRegions) {
      for (m in 1:5) {
        for (t in 1:nYears) {

      logit(mu.b[k,t,m]) <- alpha0.b[k]+beta0.b[k]*(m+2)
      shape.b[k,t,m] <- mu.b[k,t,m]*phi.b
      scale.b[k,t,m] <- (1-mu.b[k,t,m])*phi.b
      G1.b[k,t,m] ~ dgamma(shape.b[k,t,m]+0.1,1)
      G2.b[k,t,m] ~ dgamma(scale.b[k,t,m]+0.1,1)
      b[k,t,m] <- G1.b[k,t,m]/(G1.b[k,t,m]+G2.b[k,t,m])

      logit(mu.r[k,t,m]) <- alpha0.r[k]+beta0.r[k]*(m+2)
      shape.r[k,t,m] <- mu.r[k,t,m]*phi.r
      scale.r[k,t,m] <- (1-mu.r[k,t,m])*phi.r
      G1.r[k,t,m] ~ dgamma(shape.r[k,t,m]+0.1,1)
      G2.r[k,t,m] ~ dgamma(scale.r[k,t,m]+0.1,1)
      r[k,t,m] <- G1.r[k,t,m]/(G1.r[k,t,m]+G2.r[k,t,m])
}}}

    for (k in 1:nRegions) {
        for (t in 1:nYears) {

      logit(mu.juv[k,t,1]) <- omega.juv.intercept.region1
      shape.juv[k,t,1] <- mu.juv[k,t,1]*phi.juv
      scale.juv[k,t,1] <- (1-mu.juv[k,t,1])*phi.juv
      G1.juv[k,t,1] ~ dgamma(shape.juv[k,t,1],1)
      G2.juv[k,t,1] ~ dgamma(scale.juv[k,t,1],1)
      omega[k,t,1] <- G1.juv[k,t,1]/(G1.juv[k,t,1]+G2.juv[k,t,1])

      logit(mu.ad[k,t,1]) <- omega.ad.intercept.region1
      shape.ad[k,t,1] <- mu.ad[k,t,1]*phi.ad
      scale.ad[k,t,1] <- (1-mu.ad[k,t,1])*phi.ad
      G1.ad[k,t,1] ~ dgamma(shape.ad[k,t,1],1)
      G2.ad[k,t,1] ~ dgamma(scale.ad[k,t,1],1)
      omega[k,t,2] <- G1.ad[k,t,1]/(G1.ad[k,t,1]+G2.ad[k,t,1])
    }
    }

    #Create a loop across all j sites
    for(j in 1:nSites) {

    #Intitate the model for year 1 - poisson with parameter lambda
    #The abundance matrix N is specified location x year x stage

    N[j,start.year[j],1] ~ dpois(lambda[j]*1.42) #number of chicks
    N[j,start.year[j],2] ~ dpois(lambda[j]*0.10) #non-breeders
    N[j,start.year[j],3] ~ dpois(lambda[j]*1.31) #breeders

    S2[j,start.year[j],1] ~ dpois(lambda[j]*1.42*0.5)          #E[N[j,start.year[j],1]]*sjuv
    S3[j,start.year[j],1] ~ dpois(lambda[j]*0.39*0.5)          #E[N[j,start.year[j],2]]*sjuv
    S4[j,start.year[j],1] ~ dpois(lambda[j]*0.21*0.5)          #E[N[j,start.year[j],3]]*sjuv
    S4b[j,start.year[j],1] ~ dpois(lambda[j]*0.21*0.5*0.75)     #E[N[j,start.year[j],3]]*sjuv*b1
    S4nb[j,start.year[j],1] <- max(S4[j,start.year[j],1]-S4b[j,start.year[j],1],0)
    S5[j,start.year[j],1] ~ dpois(lambda[j]*0.12*0.84)          #E[N[j,start.year[j],4]]*sjuv
    S5b[j,start.year[j],1] ~ dpois(lambda[j]*0.12*0.84*0.93)     #E[N[j,start.year[j],4]]*sjuv*b1
    S5nb[j,start.year[j],1] <- max(S5[j,start.year[j],1]-S5b[j,start.year[j],1],0)
    S6[j,start.year[j],1] ~ dpois(lambda[j]*0.10*0.84)          #E[N[j,start.year[j],5]]*sjuv
    S6b[j,start.year[j],1] ~ dpois(lambda[j]*0.10*0.84*0.94)     #E[N[j,start.year[j],5]]*sjuv*b1
    S6nb[j,start.year[j],1] <- max(S6[j,start.year[j],1]-S6b[j,start.year[j],1],0)
    S7[j,start.year[j],1] ~ dpois(lambda[j]*0.09*0.84)                #E[N[j,start.year[j],7]]
    S7b[j,start.year[j],1] ~ dpois(lambda[j]*0.09*0.84*0.94)               #E[N[j,start.year[j],7]]*b2
    S7nb[j,start.year[j],1] <- max(S7[j,start.year[j],1]-S7b[j,start.year[j],1],0)
    S8[j,start.year[j],1] ~ dpois(lambda[j]*1.0*0.84)                #E[N[j,start.year[j],7]]
    S8b[j,start.year[j],1] ~ dpois(lambda[j]*1.0*0.84*0.94)               #E[N[j,start.year[j],7]]*b2
    S8nb[j,start.year[j],1] <- max(S8[j,start.year[j],1]-S8b[j,start.year[j],1],0)

#Specify the model for years 2 through nYears
    for(t in (start.year[j]+1):nYears) {

    #Estimate survivorship
    S2[j,t,1] ~ dbin(omega[region[j],t-1,1]*0.5, N[j,t-1,1]) #Survived to Age1; S2 = females only

    S3[j,t,1] ~ dbin(omega[region[j],t-1,2], S2[j,t-1,1]) # Survived to Age2

    S4[j,t,1] ~ dbin(omega[region[j],t-1,2], S3[j,t-1,1]) # Survived to Age3
    S4b[j,t,1] ~ dbin(b[region[j],t,1], S4[j,t,1])  # Survived to Age3 and bred
    S4nb[j,t,1] <- S4[j,t,1]-S4b[j,t,1]    #did not breed at Age3

    S5[j,t,1] ~ dbin(omega[region[j],t-1,2], S4[j,t-1,1]) # Survived to Age4
    S5b[j,t,1] ~ dbin(b[region[j],t,2], S5[j,t,1]) # Survived to Age4 and bred
    S5nb[j,t,1] <- S5[j,t,1]-S5b[j,t,1] #did not breed at Age4

    S6[j,t,1] ~ dbin(omega[region[j],t-1,2], S5[j,t-1,1]) # Survived to Age5
    S6b[j,t,1] ~ dbin(b[region[j],t,3], S6[j,t,1]) # Survived to Age5 and bred
    S6nb[j,t,1] <- S6[j,t,1]-S6b[j,t,1] #did not breed at Age5

    S7[j,t,1] ~ dbin(omega[region[j],t-1,2],S6[j,t-1,1]) #survived to Age6
    S7b[j,t,1] ~ dbin(b[region[j],t,4],S7[j,t,1]) #survived to Age6 and bred
    S7nb[j,t,1] <- S7[j,t,1]-S7b[j,t,1]  #did not breed at Age6

    S8[j,t,1] ~ dbin(omega[region[j],t-1,2],S7[j,t-1,1]+S8[j,t-1,1]) #survived to Age7+
    S8b[j,t,1] ~ dbin(b[region[j],t,5],S8[j,t,1]) #survived to Age7+ and bred
    S8nb[j,t,1] <- S8[j,t,1]-S8b[j,t,1]  #did not breed at Age7+

    # Number of fledglings
    G[j,t,1] ~ dbin(r[region[j],t,1], 2*S4b[j,t,1])  #fledglings from Age3
    G[j,t,2] ~ dbin(r[region[j],t,2], 2*S5b[j,t,1])  #fledglings from Age4
    G[j,t,3] ~ dbin(r[region[j],t,3], 2*S6b[j,t,1])  #fledglings from Age5
    G[j,t,4] ~ dbin(r[region[j],t,4], 2*S7b[j,t,1])  #fledglings from Age6
    G[j,t,5] ~ dbin(r[region[j],t,5], 2*S8b[j,t,1])    #fledglings Age7+

    #Sum all stages to get total N at each site j in each year t
    N[j,t,1] <- G[j,t,1]+G[j,t,2]+G[j,t,3]+G[j,t,4]+G[j,t,5]   #N1 is both males and females
    N[j,t,2] <- S2[j,t,1]+S3[j,t,1]+S4nb[j,t,1]+S5nb[j,t,1]+S6nb[j,t,1]+S7nb[j,t,1]+S8nb[j,t,1] #non breeders
    N[j,t,3] <- S4b[j,t,1]+S5b[j,t,1]+S6b[j,t,1]+S7b[j,t,1]+S8b[j,t,1]   # breeders

    growth[j,t] <- ifelse(((S4b[j,t,1]+S5b[j,t,1]+S6b[j,t,1]+S7b[j,t,1]+S8b[j,t,1])>0)&&((S4b[j,t-1,1]+S5b[j,t-1,1]+S6b[j,t-1,1]+S7b[j,t-1,1]+S8b[j,t-1,1])>0), log(S4b[j,t,1]+S5b[j,t,1]+S6b[j,t,1]+S7b[j,t,1]+S8b[j,t,1]) - log(S4b[j,t-1,1]+S5b[j,t-1,1]+S6b[j,t-1,1]+S7b[j,t-1,1]+S8b[j,t-1,1]),0)
    } # t

    mean.growth[j] <- sum(growth[j,(start.year[j]+1):nYears]) / (2014-start.year[j]) #2014 hard-coded in

    } # end of j loop

    #Loop accross reps to estimate detection probability for all years
    #The data matrix n is specified location x year x stage x rep

    for (j in 1:nSites)
      {
        for (t in start.year[j]:nYears)
          {
            for(k in 1:chick.reps[j,t])
              {
                n[j,t,k,1] ~ dnorm(N[j,t,1], 1/((N[j,t,1]+0.001)*error[acc[j,t,k,1]])^2)
              } #k
            for (k in 1:nest.reps[j,t])
              {
                n[j,t,k,2] ~ dnorm(N[j,t,3], 1/((N[j,t,3]+0.001)*error[acc[j,t,k,2]])^2)
              } #k
          } # t
      } #j

} # model

    ",fill = TRUE)
sink()


##################################
