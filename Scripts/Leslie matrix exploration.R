alpha0.r.vec <- seq(-5,-3,0.01) #-3.97
Ealpha0.r.vec<- -3.97
beta0.r.vec <- seq(0.3,0.7,0.01) #0.62
Ebeta0.r.vec<-0.62
r<-vector(length=5)
bSat.vec<-seq(0.8,1.0,0.01) #0.9485
EbSat.vec<-0.9485
alpha0.b.vec <- seq(-8,-5,0.1) #-6.14
beta0.b.vec <- seq(1.3,1.5,0.01) #1.391
EbSat.vec<-0.9485
Ealpha0.b.vec <- -6.14
Ebeta0.b.vec <- 1.391
b<-vector(length=5)
sjuv.vec <- seq(0.3,0.95,0.01) #0.5
sad.vec <- seq(0.3,0.95,0.01) #0.84
Esjuv.vec <- 0.5
Esad.vec <- 0.84
growth.rate<-array(NA,dim=c(length(sjuv.vec),length(sad.vec)))

for (i in 1:length(sjuv.vec))
  for (j in 1:length(sad.vec))
    {
      {
        alpha0.r<-Ealpha0.r.vec
        beta0.r<-Ebeta0.r.vec
        alpha0.b<-Ealpha0.b.vec
        beta0.b<-Ebeta0.b.vec
        bSat<-EbSat.vec
        r[1]<- (1/(1+exp(-(alpha0.r+beta0.r*3))))  # Age 3
        r[2]<- (1/(1+exp(-(alpha0.r+beta0.r*4))))  # Age 4
        r[3]<- (1/(1+exp(-(alpha0.r+beta0.r*5))))  # Age 5
        r[4]<- (1/(1+exp(-(alpha0.r+beta0.r*6))))  # Age 6
        r[5]<- (1/(1+exp(-(alpha0.r+beta0.r*7))))  # Age 7+
        b[1]<- (1/(1+exp(-(alpha0.b+beta0.b*3))))*bSat  # Age 3
        b[2]<- (1/(1+exp(-(alpha0.b+beta0.b*4))))*bSat  # Age 4
        b[3]<- (1/(1+exp(-(alpha0.b+beta0.b*5))))*bSat  # Age 5
        b[4]<- (1/(1+exp(-(alpha0.b+beta0.b*6))))*bSat  # Age 6
        b[5]<- (1/(1+exp(-(alpha0.b+beta0.b*7))))*bSat  # Age 7+
        sjuv<-sjuv.vec[i]
        sad<-sad.vec[j]
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
growth.rate[i,j]<-max(Re(eigen(A)$values))
}}

image(x=sjuv.vec,y=sad.vec,z=growth.rate)
contour(x=sjuv.vec,y=sad.vec,z=growth.rate,add=T,levels=c(0.6,0.7,0.8,0.9))
segments(x0=0.3,y0=0.5,x1=0.5,y1=0.3,col="white",lwd=2)
segments(x0=0.4,y0=0.8,x1=0.8,y1=0.4,col="white",lwd=2)


