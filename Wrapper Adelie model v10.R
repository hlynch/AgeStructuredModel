
##############################################################
#
# START OF DATA PREP for "Adelie simplified life cycle.R"
#
#############################################################


#setwd("W:\\Adelie state space modelling\\State space 2015\\")
#jags.path="C:\\Program Files\\JAGS\\JAGS-3.4.0\\x64\\bin\\"
#setwd("/Volumes/LYNCH USB/Adelie state space modelling/state space 2015/")
setwd("~/Desktop/Adelie state space modelling/State space 2015/")
jags.path="/usr/local/bin/jags/"
library('R2jags')
library('snowfall')

###Read in all of the data and format it

chick.data<-read.csv("Data/Adelie chick data for JWP Center collapsed updated PALM deleted.csv",header=T)
nest.data<-read.csv("Data/Adelie nest data for JWP Center collapsed updated PALM deleted.csv",header=T)

# Determine the maximal number of repeated counts within a year
max.rep <- max(c(chick.data$REPEAT, nest.data$REPEAT))

# Unique nest count data (might have to change if data changes)

usite=as.character(sort(unique(nest.data$SITE)))

# Site by year by repeated count
chick.count<-array(dim=c(20,36,max.rep))
chick.date<-array(dim=c(20,36,max.rep))
chick.acc<-array(dim=c(20,36,max.rep))
nest.count<-array(dim=c(20,36,max.rep))
nest.date<-array(dim=c(20,36,max.rep))
nest.acc<-array(dim=c(20,36,max.rep))
dimnames(chick.count)= list(usite,1979:2014,1:max.rep)
dimnames(nest.count)= list(usite,1979:2014,1:max.rep)



for (i in 1:nrow(chick.data)){
  chick.count[chick.data$ROW[i],chick.data$COLUMN[i],chick.data$REPEAT[i]]<-chick.data$COUNT[i]
  chick.date[chick.data$ROW[i],chick.data$COLUMN[i],chick.data$REPEAT[i]]<-chick.data$JULIAN[i]
  chick.acc[chick.data$ROW[i],chick.data$COLUMN[i],chick.data$REPEAT[i]]<-chick.data$ACCURACY[i]
  }

for (i in 1:nrow(nest.data)){
  nest.count[nest.data$ROW[i],nest.data$COLUMN[i],nest.data$REPEAT[i]]<-nest.data$COUNT[i]
  nest.date[nest.data$ROW[i],nest.data$COLUMN[i],nest.data$REPEAT[i]]<-nest.data$JULIAN[i]
  nest.acc[nest.data$ROW[i],nest.data$COLUMN[i],nest.data$REPEAT[i]]<-nest.data$ACCURACY[i]
  }

n<-array(dim=c(20,36,max.rep,2))
n[,,,1]=chick.count
n[,,,2]=nest.count

acc<-array(dim=c(20,36,max.rep,2))
acc[,,,1]=chick.acc
acc[,,,2]=nest.acc
acc[is.na(acc)] <- 1

error <- c(0.025, 0.05, 0.075, 0.25)

nSites=dim(nest.count)[1]
nYears=dim(nest.count)[2]

# Compute the number of repeated counts per year and site for each data set
nest.reps <- matrix(NA, nrow=dim(nest.count)[1], ncol=dim(nest.count)[2])

for (j in 1:dim(nest.count)[1])
  {
    for (i in 1:dim(nest.count)[2])
      {
        nest.reps[j,i] <- sum(!is.na(nest.count[j,i,]))
      }
  }
nest.reps[nest.reps==0]=1

chick.reps <- matrix(NA, nrow=dim(chick.count)[1], ncol=dim(chick.count)[2])

for (j in 1:dim(chick.count)[1])
  {
    for (i in 1:dim(chick.count)[2])
      {
        chick.reps[j,i] <- sum(!is.na(chick.count[j,i,]))
      }
  }
chick.reps[chick.reps==0]=1


# Create the region variable
table(nest.data$REGIONINDEX, nest.data$SITE)
table(chick.data$REGIONINDEX, chick.data$SITE)
region <- c(1,1,3,3,3,3,4,2,4,3,1,3,1,1,3,1,3,3,5,5)
nRegions <- length(unique(region))

# Create the site-specific starting year - this will throw up warnings() but it doesn't affect reuslts
start.year1 <- start.year2 <- start.year <- numeric()
for (j in 1:20)
  {
    start.year1[j] <- min(which(!is.na(n[j,,1,1])))
    start.year2[j] <- min(which(!is.na(n[j,,1,2])))
    start.year[j] <- min(c(start.year1[j], start.year2[j]))
  }

# Create a vector to intiate the first year of sampling
lambda <- numeric()
for (j in 1:nSites)
  {
    lambda[j] <- max(c(n[j,start.year[j],1,1],n[j,start.year[j],1,2]), na.rm=T)
  }


###################################

# Calculation of the stable age distribution

alpha0.r <- -3.97
beta0.r <- 0.62

r<-vector(length=5)
r[1]<- (1/(1+exp(-(alpha0.r+beta0.r*3))))  # Age 3
r[2]<- (1/(1+exp(-(alpha0.r+beta0.r*4))))  # Age 4
r[3]<- (1/(1+exp(-(alpha0.r+beta0.r*5))))  # Age 5
r[4]<- (1/(1+exp(-(alpha0.r+beta0.r*6))))  # Age 6
r[5]<- (1/(1+exp(-(alpha0.r+beta0.r*7))))  # Age 7+

mu.bSat<-0.9485
var.bSat<-0.073*0.073
al.bSat<-mu.bSat*(mu.bSat*(1-mu.bSat)/var.bSat-1)
be.bSat<-(1-mu.bSat)*(mu.bSat*(1-mu.bSat)/var.bSat-1)

bSat<-1
alpha0.b <- -6.14
beta0.b <- 2.5

b<-vector(length=5)
b[1]<- (1/(1+exp(-(alpha0.b+beta0.b*3))))*bSat  # Age 3
b[2]<- (1/(1+exp(-(alpha0.b+beta0.b*4))))*bSat  # Age 4
b[3]<- (1/(1+exp(-(alpha0.b+beta0.b*5))))*bSat  # Age 5
b[4]<- (1/(1+exp(-(alpha0.b+beta0.b*6))))*bSat  # Age 6
b[5]<- (1/(1+exp(-(alpha0.b+beta0.b*7))))*bSat  # Age 7+

sjuv <- 0.5
sad <- 0.84


A <- matrix(c(0,    0,    0,   r[1]*b[1], r[2]*b[2],  r[3]*b[3], r[4]*b[4],     r[5]*b[5],
              sjuv, 0,    0,       0,          0,           0,   0,          0,
              0,    sjuv, 0,       0,          0,           0,   0,          0,
              0,    0,    sjuv,    0,          0,           0,   0,          0,
              0,    0,    0,       sad,        0,           0,   0,          0,
              0,    0,    0,       0,          sad,         0,   0,          0,
              0,    0,    0,       0,          0,          sad,  0,          0,
              0,    0,    0,       0,          0,           0,   sad,       sad),
            ncol = 8, byrow = T)

# Population growth rate
max(Re(eigen(A)$values))

# Calculate the stable age distribution
z <- which(Re(eigen(A)$values)==max(Re(eigen(A)$values)))
revec <- Re(eigen(A)$vectors[,z])
age <- matrix(revec/sum(revec)) # standardised stable age distribution

# Restandardize the age distribution with respect to the class of the adult breeders
age[1] <- 2*age[1]   # adjustment, because we count the total number of chicks, and not just the females
age <- age/sum(age)
age <- age/age[8]
age


###################################

#  Bundle the data for JAGS

Dat <- list(nSites = nSites, nYears = nYears, nRegions=nRegions, n = n, start.year = start.year, region=region,
            chick.reps = chick.reps, nest.reps = nest.reps, lambda = lambda, acc = acc, error = error)

##################################

# Compute the indices of those population parameters that might be monitored

# Index for the population estimates that should to stored
mat <- matrix(NA, ncol=nYears, nrow=nSites)
for (j in 1:nSites){
   for (t in (start.year[j]+1):nYears){
      mat[j,t] <- 1
      }
   }
ind.site <- which(mat==1, arr.ind=T)[,1]
ind.year <- which(mat==1, arr.ind=T)[,2]
N.monitor1 <- N.monitor2 <- N.monitor3 <- numeric()
for (i in 1:length(ind.site))
{
  N.monitor1[i] <- paste("N[",ind.site[i],",",ind.year[i],",1]", sep="")
  N.monitor2[i] <- paste("N[",ind.site[i],",",ind.year[i],",2]", sep="")
  N.monitor3[i] <- paste("N[",ind.site[i],",",ind.year[i],",3]", sep="")
}

N.monitor <- c(N.monitor1,N.monitor2,N.monitor3)


# Index for the population growth rates that cound be stored
mat <- matrix(NA, ncol=nYears, nrow=nSites)
for (j in 1:nSites){
   for (t in (start.year[j]+1):nYears){
      mat[j,t] <- 1
      }
   }
ind.site <- which(mat==1, arr.ind=T)[,1]
ind.year <- which(mat==1, arr.ind=T)[,2]
growth <- numeric()
for (i in 1:length(ind.site)){
   growth[i] <- paste("growth[",ind.site[i],",",ind.year[i],"]", sep="")
   }

##############################################

# Compute values for initial parameters

Nnew <- array(NA, dim = c(nSites, nYears, 3))
S2new<- array(NA, dim = c(nSites, nYears, 1))
S3new<- array(NA, dim = c(nSites, nYears, 1))
S3bnew<- array(NA, dim = c(nSites, nYears, 1))
S4new<- array(NA, dim = c(nSites, nYears, 1))
S4bnew<- array(NA, dim = c(nSites, nYears, 1))
S5new<- array(NA, dim = c(nSites, nYears, 1))
S5bnew<- array(NA, dim = c(nSites, nYears, 1))
S6new<- array(NA, dim = c(nSites, nYears, 1))
S6bnew<- array(NA, dim = c(nSites, nYears, 1))
S7new<- array(NA, dim = c(nSites, nYears, 1))
S7bnew<- array(NA, dim = c(nSites, nYears, 1))
S8new<- array(NA, dim = c(nSites, nYears, 1))
S8bnew<- array(NA, dim = c(nSites, nYears, 1))

for (j in 1:nSites){
  Nnew[j,start.year[j],1] <- round(lambda[j]*1.42)  #number of chicks
  Nnew[j,start.year[j],2] <- round(lambda[j]*0.10)  #stage 1 (non-breeders)
  Nnew[j,start.year[j],3] <- round(lambda[j]*1.31)  #stage 2 (might or might not breed)

  S2new[j,start.year[j],1] <- round(lambda[j]*1.42*0.5)
  S3new[j,start.year[j],1] <- round(lambda[j]*0.39*0.5)
  S4new[j,start.year[j],1] <- round(lambda[j]*0.21*0.5)
  S4bnew[j,start.year[j],1] <- round(lambda[j]*0.21*0.5*0.75)
  S5new[j,start.year[j],1] <- round(lambda[j]*0.12*0.84)
  S5bnew[j,start.year[j],1] <- round(lambda[j]*0.12*0.84*0.93)
  S6new[j,start.year[j],1] <- round(lambda[j]*0.10*0.84)
  S6bnew[j,start.year[j],1] <- round(lambda[j]*0.10*0.84*0.94)
  S7new[j,start.year[j],1] <- round(lambda[j]*0.09*0.84)
  S7bnew[j,start.year[j],1] <- round(lambda[j]*0.09*0.84*0.94)
  S8new[j,start.year[j],1] <- round(lambda[j]*1.0*0.84)
  S8bnew[j,start.year[j],1] <- round(lambda[j]*1.0*0.84*0.94)
}

InitStage <- function() list(N = Nnew, S2=S2new, S3=S3new,S4=S4new,S4b=S4bnew,S5=S5new,S5b=S5bnew,S6=S6new,S6b=S6bnew,S7=S7new,S7b=S7bnew, S8=S8new, S8b=S8bnew)


##############################################################
#
# END OF DATA PREP
#
#############################################################


ParsStage <- c("alpha0.b[1]",
               "alpha0.b[2]",
               "alpha0.b[3]",
               "alpha0.b[4]",
               "alpha0.b[5]",
               "alpha0.r[1]",
               "alpha0.r[2]",
               "alpha0.r[3]",
               "alpha0.r[4]",
               "alpha0.r[5]",
               "beta0.b[1]",
               "beta0.b[2]",
               "beta0.b[3]",
               "beta0.b[4]",
               "beta0.b[5]",
               "beta0.r[1]",
               "beta0.r[2]",
               "beta0.r[3]",
               "beta0.r[4]",
               "beta0.r[5]",
               "omega.juv.intercept.region1",
               "omega.juv.intercept.region2",
               "omega.juv.intercept.region3",
               "omega.juv.intercept.region4",
               "omega.juv.intercept.region5",
               "omega.ad.intercept.region1",
               "omega.ad.intercept.region2",
               "omega.ad.intercept.region3",
               "omega.ad.intercept.region4",
               "omega.ad.intercept.region5",
               "phi.b",
               "phi.r",
               "phi.ad",
               "phi.juv",
               "omega",
               "mean.growth[1]",
               "mean.growth[2]",
               "mean.growth[3]",
               "mean.growth[4]",
               "mean.growth[5]",
               "mean.growth[6]",
               "mean.growth[7]",
               "mean.growth[8]",
               "mean.growth[9]",
               "mean.growth[10]",
               "mean.growth[11]",
               "mean.growth[12]",
               "mean.growth[13]",
               "mean.growth[14]",
               "mean.growth[15]",
               "mean.growth[16]",
               "mean.growth[17]",
               "mean.growth[18]",
               "mean.growth[19]",
               "mean.growth[20]",
               "r",
               "b")

# MCMC settings
ni <- 1000000
nt <- 100
nb <- 500000
nc <- 3

ni <- 70000
nt <- 50
nb <- 20000
nc <- 3

#######################################################
# Model 15 SERIAL EXECUTION
#######################################################

system.time(m <- jags(Dat, InitStage, ParsStage, "Adelie simplified life cycle v10.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd()))

jagsfit.mcmc <- as.mcmc(m[[2]])  #m[[2]] is the JAGSoutput
save(jagsfit.mcmc,file="August9ModelRun1.Rdata")

options(scipen=5)
sink("August10ModelRun1_summary.txt")
print(jagsfit.mcmc$summary[,c(1,3,7,8)],digits=3)
sink()

#######################################################
# Model 15 PARALLEL EXECUTION
#######################################################

library(abind)
library(R2WinBUGS)
JAGSParallelized = function(Index, jags.seed.vec, inits, model.file, data, working.directory=NULL, params, n.thin, n.iter, n.burnin, DIC){
  RNGset = c("Mersenne-Twister","Marsaglia-Multicarry","Super-Duper","Knuth-TAOCP-2002","Knuth-TAOCP","Wichmann-Hill","L'Ecuyer-CMRG")
  set.seed(1,kind=RNGset[Index])
  random.seed = runif(1,1,1e6)
  Jags = jags(inits=inits, n.chains=1, model.file=model.file, working.directory=working.directory, data=data, parameters.to.save=params, n.thin=n.thin, n.iter=n.iter, n.burnin=n.burnin, DIC=DIC)
  return(Jags)
}

#jags.seed=random.seed,

JAGSParallel = function(n.cores,data,inits,params,model.file,debug,n.chains,n.iter,n.burnin,n.thin,DIC=FALSE){
  # Start snowfall
  sfInit(parallel=TRUE, cpus=n.cores)
  sfLibrary(R2jags)
  sfLibrary(snowfall)
  sfExportAll()
  sfClusterCall( runif, 4 )
  jags.seed.vec = ceiling(runif(n.chains,1,1e6))
  # Run JAGS
  JAGSList <- sfLapply(1:n.chains, JAGSParallelized, jags.seed.vec =jags.seed.vec, data=data, inits=inits, params=params,
                       model.file= model.file, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin, DIC=DIC)
  # End snowfall
  sfStop()
  result <- NULL
  model <- NULL
  for (ch in 1:n.chains) {
    result <- abind(result, JAGSList[[ch]]$BUGSoutput$sims.array, along = 2)
    model[[ch]] <- JAGSList[[ch]]$model
  }
  result <- as.bugs.array(result, model.file = model.file,program = "jags", DIC = DIC, n.iter = n.iter, n.burnin = n.burnin,n.thin = n.thin)
  out <- list(model = model, JAGSoutput = result, parameters.to.save = params, model.file = model.file, n.iter = n.iter, DIC = DIC)
  class(out) <- c("rjags.parallel", "rjags")
  return(out)
}

system.time(m<-JAGSParallel(3,data=Dat, inits=InitStage, params=ParsStage, model.file= "Adelie simplified life cycle v10.jags", n.chains=nc, n.iter=ni, n.burnin=nb, n.thin=nt))
jagsfit.mcmc <- as.mcmc(m[[2]])  #m15[[2]] is the JAGSoutput
save(jagsfit.mcmc,file="August10ModelRun1.Rdata")



