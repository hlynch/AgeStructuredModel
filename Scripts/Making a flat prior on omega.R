omega<-c()
for (i in 1:10000)
{
  intercept<-rnorm(1,0,sqrt(1/0.67))
  sigma.year<-runif(1,0,2.5)
  lomega<-rnorm(1,intercept, sigma.year)
  omega<-c(omega,inv.logit(lomega))
}