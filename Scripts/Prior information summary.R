######################
#
# Breeding success
#
######################

# Clarke et al. 2003
# 0.71 chicks creched per nest
chicks<-c(0.84,0.71,0.80,1.06,0.02,0.40,0.93,0.80,0.38,0.54,0.87,1.03) #chicks per nest
chicks<-chicks/2 #chicks per parent

# Ainley 1983 - Table 8.16
# Breeding success by age

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
plot(age,chicks,ylim=c(0,1.0))
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
lines(age,inv.logit(alpha0+beta0*age),col="gray")

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
plot(age,chicks,ylim=c(0,1.0))
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
lines(age,inv.logit(alpha0+beta0*age),col="gray")

#> alpha0
#[1] -3.100947
#> beta0
#[1] 0.3917

# Hinke thesis
# how much interannual variation in adult breeding success?

year<-seq(1986,2008,1)
breeding.success<-c(1.17,0.59,0.91,0.55,0.31,0.73,0.75,1.18,0.95,1.26,0.62,1.49,1.39,0.95,1.28,0.50,0.92,0.87,1.01,1.03,0.92,0.41,1.06)

######################
#
# Probability of breeding
#
######################

# From Ainley and DeMaster
# Approximate numbers taken from Fig. 7

age<-seq(2,7,1)
breeding.prop<-c(0,5.9,21.1,60.9,82.4,89.8)
breeding.prop<-breeding.prop/100
fit<-glm(breeding.prop~age,family="binomial")

# From Ballerini et al. (2009)
#  breeding probability ~0.98

#From Clarke et al. 2003
age<-seq(2,7,1)
breeding.prop<-c(0,3.8,11.0,18.9,36.8,43.7)
breeding.prop<-breeding.prop/100
fit<-glm(breeding.prop~age,family="binomial")

# alpha0 = -5.33
# beta0 = 0.7582

# Ainley 1983 - Table 6.5
# Probability of breeding in Adelies (females) of known age

age<-seq(2,8,1)
breeding.prop<-c(0,17,49,68,82,90,91)
breeding.prop<-breeding.prop/100
fit<-glm(breeding.prop~age,family="binomial")

fit<-nls(breeding.prop~inv.logit(alpha0+beta0*age)*bSat,start=list(alpha0=-5.5,beta0=2.5,bSat=0.91))
plot(age,breeding.prop,ylim=c(0,1.0))
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
bSat<-summary(fit)$coefficients[3,1]
lines(age,inv.logit(alpha0+beta0*age)*bSat,col="gray")

#> alpha0
#[1] -5.528893
#> beta0
#[1] 1.379213
#> bSat
#[1] 0.8973632

#for (i in 1:100)
#{
#  alpha0<-rnorm(1,summary(fit)$coefficients[1,1],summary(fit)$coefficients[1,2])
#  beta0<-rnorm(1,summary(fit)$coefficients[2,1],summary(fit)$coefficients[2,2])
#  bSat<-rnorm(1,summary(fit)$coefficients[3,1],summary(fit)$coefficients[3,2])
#  lines(age,inv.logit(alpha0+beta0*age)*bSat,col="gray")
#}
points(age,breeding.prop,pch=15,cex=1.5)

# Hinke thesis - Table 3-1
# Breeding propensity

age<-c(2,3,4,5,6,7) #7=7+
breeding.prop<-c(0,0.05,0.33,0.46,0.94,0.94)

# fit with bSat has bSat=1.04 so it was fixed at 1.0
fit<-nls(breeding.prop~inv.logit(alpha0+beta0*age),start=list(alpha0=-5.5,beta0=1.2))
plot(age,breeding.prop,ylim=c(0,1.0))
alpha0<-summary(fit)$coefficients[1,1]
beta0<-summary(fit)$coefficients[2,1]
lines(age,inv.logit(alpha0+beta0*age),col="gray")

#> alpha0
#[1] -6.746821
#> beta0
#[1] 1.403465


######################
#
# Survival
#
######################

# Ainley and DeMaster 1980
# Table 4

# adult survival, all adult age classes and time periods lumped
sad<-c(0.788,0.809,0.688,0.801,0.916,0.800,0.659,0.599,0.645,0.639,0.571,0.418,0.564,0.706,0.833)


#Ballerini et al.
# Extracted from Fig. 3
sad<-c(0.759,0.820,0.839,0.898,0.810,0.938,0.882,0.882,0.830,0.830)
sjuv<-0.34 #from Edmondson Point
#sjuv from Ross Island, Reid 1968=0.52
#sjuv from Ross Island, Ainley et al. 1983=0.29-0.51
#sjuv from Becharvaise Island Clarke et al. 2003 0.69-0.77

# Clarke et al. 2003
sad <- 0.862 #pooled estimate
age<-c(3,4,5,6,7)
sad<-c(0.865,0.872,0.869,0.905,0.795) #age specific

# Ainley 1983 - Table 9.11
# Adult survivorship

# Not strong evidence in this dataset for a change in survivorship with age
# lowest s=0.789 and highest s=0.971
# This gives me some bounds for a prior on omega

# Ainley 1983 - Table 9.12
# Juvenile survivorship

# Survival 0->1 and 1->2 is estimated at 0.513

# Hinke thesis
# Juvenile survivorship

s.juv<-c(0.42,0.40,0.42,0.63,0.74,0.35,0.49,0.57,0.40,0.44,0.53,0.34,0.68,0.39,0.40,0.46,0.58,0.32,0.60,0.74,0.38,0.59,0.58,0.58) # age 0-2

#> mean(s.juv)
#[1] 0.50125  # NB: This is almost exactly what Ainley found
#> sd(s.juv)
#[1] 0.1255704

s.ad<-c(0.84,0.84,0.96,0.92,0.71,0.71,0.94,0.94,0.90,0.84,0.92,0.75,0.92,0.94,0.70,0.84,0.81,0.89,0.68,0.80,0.74,0.89,0.87,0.86) # age 3+

#> mean(s.ad)
#[1] 0.8420833
#> sd(s.ad)
#[1] 0.08672692