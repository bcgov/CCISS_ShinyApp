
#####################
####### theoretical basis for the Chi Metric: a dimension-independent metric with consistent statistical meaning 
####################

require(adehabitatLT)   #provides calculation of the chi distribution
# require(fdrtool)
require(plotrix)
require(FNN)
options(digits=10)  #increase the defualt signif digits to 10 from 7. necessary to avoid rounding the sigma 6 percentile to 1. 


###Simulation plot showing the geometric basis for the relationship between dimensionality, distance, and probability. 
png(filename=paste("instructions-raw/Figure_Novelty_SigmaDemo.png",sep=""), type="cairo", units="in", width=9, height=4.5, pointsize=12, res=300)
mat <- matrix(c(1,1,2,3), 2)
layout(mat)
N=100
par(xpd=FALSE)
par(mar=c(3,3,1,1), mgp=c(2,1,0))
plot(0,0, xlim=c(-3,3),ylim=c(-3,3), col="white", xlab="z-score (standard deviations)", ylab="z-score (standard deviations)")
# rect(-1,-4,1,4, col="lightgrey", lty=0)
# rect(-4,-1,4,1, col="lightgrey", lty=0)
points(rnorm(n=N, m=0, sd=1),rnorm(n=N, m=0, sd=1), cex=1, col="grey", pch=16)
# points(0,2.75, pch=16); arrows(0,2.75,1,2.75, code=2, length=0.1, lwd=2); text(0.5, 2.95, "d=1", font=2)
draw.circle(0,0,1, lty=2); 
# points(0,0, pch=16); arrows(0,0,sin(0.75*pi),sin(0.75*pi), code=2, length=0.1, lwd=2); text(0.25, 0.25, "r=1", pos=4, font=2)
arrows(-1,-3,1,-3, code=3, length=0.05, lwd=1); text(0, -3, "68% probability\nof distance<1\nin one dimension", pos=3, cex=1)
arrows(sin(0.75*pi),cos(0.75*pi),1.5,-1.5, code=1, length=0.05, lwd=1); text(1.15, -2.05, "39% probability\nof distance<1\nin two dimensions", pos=4, cex=1)
lines(c(-1,-1),c(-4,4), lty=2); lines(c(1,1),c(-4,4), lty=2)
text(-3,2.2, "random normal\nsample (n=100)", pos=4, col="gray", font=2)
mtext(paste0("(", letters[1], ")"), side=3, line=-1.5, adj = 0.025, font=2)
box()


#######################################
## plot of sigma levels on a chi-square distribution for various dimensionalities, INCLUDING RANDOM SAMPLE 
sigma <- c(1,2,3,4,5,6)
pctile <- pchi(sigma,1)
dims <- c(1,2)
dec <- c(0,0,1,3,5,7)
N=10000
x <- seq(0,9.99,0.01)

# par(mgp=c(3,1,0))
par(mgp=c(2,0.5,0))

for(i in dims){
  if(i == 1) par(mar=c(2,2,1,0.1)) else par(mar=c(3,2,0.1,0.1)) 
  y <- dchi(x,df=i)
plot(x,y, xaxs="i", yaxs="i", ylim=c(0,1.7*max(y)),yaxt='n',col="white",xlab="",ylab="")
par(mgp=c(0.5,0,0)); title(ylab="Probability density"); par(mgp=c(2,0.5,0))
simdata <- matrix(rnorm(n=N*i, m=0, sd=1),N,i)
dist <- as.vector(get.knnx(data=matrix(apply(simdata,2,mean),1,i),query=simdata,k=1,algorithm="brute")[[2]]) 
hist(dist, xlim=c(0,max(x)), ylim=c(0,1), main="", xlab="", ylab="Probability density", col="gray", border=NA,freq=F, breaks=seq(0,max(x),0.5), add=T)
lines(x,y,lwd=2)
# legend("topright",legend =paste(i,"dimension"),bty="n",cex=1.1)
sigma.x <- qchi(pchi(sigma,1),i)
for(k in sigma){
    ypos <- 1.2*max(y)-sigma[k]*0.175*max(y)
    lines(c(sigma.x[k],sigma.x[k]),c(0,ypos-0.05),lty=2)  
    if(i%in%dims[1:2]){
      if(k==1){text(sigma.x[k]-0.35,ypos,bquote(.(sigma[k])*{sigma}~(.(round(pchi(sigma,1)[k]*100,dec[k]))^th~percentile)), cex=1, font=2, pos=4)}
      else{text(sigma.x[k]-0.35,ypos,bquote(.(sigma[k])*{sigma}~(.(round(pchi(sigma,1)[k]*100,dec[k]))^th~"%"*ile)), cex=1, font=2, pos=4)}}
    else text(sigma.x[k]-0.35,ypos,bquote(.(sigma[k])*{sigma}), cex=1, font=2, pos=4)                    
  }
if(i%in%dims[1]){text(0, 1.55*max(y),paste(i,"dimension"), pos=4, font=2, cex=1.2)} else{text(0, 1.55*max(y),paste(i,"dimensions"), pos=4, font=2, cex=1.2)}
legend("topright", legend="random normal sample\n(n=10000)", y.intersp=1.2, pch=c(22), pt.bg=c("gray"), pt.cex=c(3), col=NA, cex=1, bty="n")
if(i%in%dims[1]){z=70;text(x[z], 1.45*y[z],bquote(chi[nu==.(i)]), font=2, pos=3, cex=1.8)} else{text(x[which(y==max(y))], 1.15*max(y),bquote(chi[nu==.(i)]), font=2, pos=3, cex=1.8)}
if(i%in%dims[1]){arrows(x[z/2],y[z/2],x[z/2],1.46*y[z], code=1, length=0.1)} else{arrows(x[which(y==max(y))],max(y),x[which(y==max(y))],1.16*max(y), code=1, length=0.1)}
# if(i%in%dims[1]){text(0, 1.2*max(y),bquote(chi[nu==.(i)]~("chi"~distribution~with~.(i)~"degree"~of~freedom)), pos=4, font=2, cex=1.5)} else{text(x[which(y==max(y))], 1.2*max(y),bquote(chi[nu==.(i)]), font=2, cex=1.5)}
mtext(paste0("(", letters[i+1], ")"), side=3, line=-1, adj = -0.065, font=2)
}
title(xlab="non-squared Mahalanobis distance to sample mean (centroid)")
dev.off()
