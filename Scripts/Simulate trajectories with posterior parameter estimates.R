library(boot)

# simulate only Region 1, this code can work as a stand in for a "generic" site
Region<-5
nYears=100
Breeders<-array(dim=c(nYears,1000))

for (i in 1:ncol(Breeders))   #each loop represents sample from the joint posterior
{
b<-r<-array(dim=c(nYears,5))
omega<-array(dim=c(nYears,2))
for (t in 1:nYears)
  {
  which.year<-sample(seq(1,36),1)
  omega[t,1]<-jagsfit.mcmc$sims.matrix[i,which(colnames(jagsfit.mcmc$sims.matrix)==paste("omega[",as.character(Region),",",as.character(which.year),",1]",sep=""))]
  omega[t,2]<-jagsfit.mcmc$sims.matrix[i,which(colnames(jagsfit.mcmc$sims.matrix)==paste("omega[",as.character(Region),",",as.character(which.year),",2]",sep=""))]
  for (j in 1:5)
        {
          temp.string<-paste("[",as.character(Region),",",as.character(which.year),",",as.character(j),"]",sep="")
          b[t,j] <- jagsfit.mcmc$sims.matrix[i,which(colnames(jagsfit.mcmc$sims.matrix)==paste("b",temp.string,sep=""))]
          r[t,j] <- jagsfit.mcmc$sims.matrix[i,which(colnames(jagsfit.mcmc$sims.matrix)==paste("r",temp.string,sep=""))]
         }
  }

    #Create a loop across all j sites

    #Intitate the model for year 1 - poisson with parameter lambda
    #The abundance matrix N is specified location x year x stage
N <- array(NA, dim = c(nYears, 3))
S2<- vector(length = nYears)
S3<- vector(length = nYears)
S3b<- vector(length = nYears)
S3nb<- vector(length = nYears)
S4<- vector(length = nYears)
S4b<- vector(length = nYears)
S4nb<- vector(length = nYears)
S5<- vector(length = nYears)
S5b<- vector(length = nYears)
S5nb<- vector(length = nYears)
S6<- vector(length = nYears)
S6b<- vector(length = nYears)
S6nb<- vector(length = nYears)
S7<- vector(length = nYears)
S7b<- vector(length = nYears)
S7nb<- vector(length = nYears)
S8<- vector(length = nYears)
S8b<- vector(length = nYears)
S8nb<- vector(length = nYears)
G <- array(dim=c(nYears,5))


lambda<-1000
    N[1,1] <- rpois(1,lambda*1.42) #number of chicks
    N[1,2] <- rpois(1,lambda*0.10) #non-breeders
    N[1,3] <- rpois(1,lambda*1.31) #breeders

    S2[1] <- rpois(1,lambda*1.42*0.5)          #E[N[j,start.year[j],1]]*sjuv
    S3[1] <- rpois(1,lambda*0.39*0.5)          #E[N[j,start.year[j],2]]*sjuv
    S4[1] <- rpois(1,lambda*0.21*0.5)          #E[N[j,start.year[j],3]]*sjuv
    S4b[1] <- rpois(1,lambda*0.21*0.5*0.75)     #E[N[j,start.year[j],3]]*sjuv*b1
    S4nb[1] <- max(S4[1]-S4b[1],0)
    S5[1] <- rpois(1,lambda*0.12*0.84)          #E[N[j,start.year[j],4]]*sjuv
    S5b[1] <- rpois(1,lambda*0.12*0.84*0.93)     #E[N[j,start.year[j],4]]*sjuv*b1
    S5nb[1] <- max(S5[1]-S5b[1],0)
    S6[1] <- rpois(1,lambda*0.10*0.84)          #E[N[j,start.year[j],5]]*sjuv
    S6b[1] <- rpois(1,lambda*0.10*0.84*0.94)     #E[N[j,start.year[j],5]]*sjuv*b1
    S6nb[1] <- max(S6[1]-S6b[1],0)
    S7[1] <- rpois(1,lambda*0.09*0.84)                #E[N[j,start.year[j],7]]
    S7b[1] <- rpois(1,lambda*0.09*0.84*0.94)               #E[N[j,start.year[j],7]]*b2
    S7nb[1] <- max(S7[1]-S7b[1],0)
    S8[1] <- rpois(1,lambda*1.0*0.84)                #E[N[j,start.year[j],7]]
    S8b[1] <- rpois(1,lambda*1.0*0.84*0.94)               #E[N[j,start.year[j],7]]*b2
    S8nb[1] <- max(S8[1]-S8b[1],0)

#Specify the model for years 2 through nYears
    for(t in 2:nYears) {

    #Estimate survivorship
    S2[t] <- rbinom(1,N[t-1,1],omega[t-1,1]*0.5) #Survived to Age1; S2 = females only

    S3[t] <- rbinom(1,S2[t-1],omega[t-1,2]) # Survived to Age2

    S4[t] <- rbinom(1,S3[t-1],omega[t-1,2]) # Survived to Age3
    S4b[t] <- rbinom(1,S4[t],b[t,1])  # Survived to Age3 and bred
    S4nb[t] <- S4[t]-S4b[t]    #did not breed at Age3

    S5[t] <- rbinom(1,S4[t-1],omega[t-1,2]) # Survived to Age4
    S5b[t] <- rbinom(1,S5[t],b[t,2]) # Survived to Age4 and bred
    S5nb[t] <- S5[t]-S5b[t] #did not breed at Age4

    S6[t] <- rbinom(1,S5[t-1],omega[t-1,2]) # Survived to Age5
    S6b[t] <- rbinom(1,S6[t],b[t,3]) # Survived to Age5 and bred
    S6nb[t] <- S6[t]-S6b[t] #did not breed at Age5

    S7[t] <- rbinom(1,S6[t-1],omega[t-1,2]) #survived to Age6
    S7b[t] <- rbinom(1,S7[t],b[t,4]) #survived to Age6 and bred
    S7nb[t] <- S7[t]-S7b[t]  #did not breed at Age6

    S8[t] <- rbinom(1,S7[t-1]+S8[t-1],omega[t-1,2]) #survived to Age7+
    S8b[t] <- rbinom(1,S8[t],b[t,5]) #survived to Age7+ and bred
    S8nb[t] <- S8[t]-S8b[t]  #did not breed at Age7+

    # Number of fledglings
    G[t,1] <- rbinom(1,2*S4b[t],r[t,1])  #fledglings from Age3
    G[t,2] <- rbinom(1,2*S5b[t],r[t,2])  #fledglings from Age4
    G[t,3] <- rbinom(1,2*S6b[t],r[t,3])  #fledglings from Age5
    G[t,4] <- rbinom(1,2*S7b[t],r[t,4])  #fledglings from Age6
    G[t,5] <- rbinom(1,2*S8b[t],r[t,5])    #fledglings Age7+

    #Sum all stages to get total N at each site j in each year t
    N[t,1] <- G[t,1]+G[t,2]+G[t,3]+G[t,4]+G[t,5]   #N1 is both males and females
    N[t,2] <- S2[t]+S3[t]+S4nb[t]+S5nb[t]+S6nb[t]+S7nb[t]+S8nb[t] #non breeders
    N[t,3] <- S4b[t]+S5b[t]+S6b[t]+S7b[t]+S8b[t]   # breeders

    } # t

    #Loop accross reps to estimate detection probability for all years
    #The data matrix n is specified location x year x stage x rep

Breeders[,i]<-N[,3]
}

plot(Breeders[,1],ylim=c(0,4000),typ="l",col=rgb(0,0,0,alpha=0.05))
for (i in 2:ncol(Breeders))
{
  lines(Breeders[,i],col=rgb(0,0,0,alpha=0.05))
}
LL<-c()
UL<-c()
for (i in 1:nrow(Breeders))
{
  LL<-c(LL,quantile(Breeders[i,],probs=0.25))
  UL<-c(UL,quantile(Breeders[i,],probs=0.75))
}
lines(LL,col="black",lwd=3)
lines(UL,col="black",lwd=3)

lines(Breeders[,5],col="red",lwd=2)
lines(Breeders[,6],col="orange",lwd=2)


